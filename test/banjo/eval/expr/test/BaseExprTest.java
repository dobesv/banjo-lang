package banjo.eval.expr.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import banjo.eval.Fail;
import banjo.eval.environment.Environment;
import banjo.eval.environment.LetEnvironment;
import banjo.eval.environment.ProjectEnvironment;
import banjo.expr.core.BaseCoreExprVisitor;
import banjo.expr.core.Call;
import banjo.expr.core.CoreErrorGatherer;
import banjo.expr.core.CoreExpr;
import banjo.expr.core.Let;
import banjo.expr.core.SlotReference;
import banjo.expr.free.FreeExpression;
import banjo.expr.free.FreeExpressionFactory;
import banjo.expr.util.ListUtil;
import banjo.value.Value;
import fj.P;
import fj.P2;
import fj.data.List;

public abstract class BaseExprTest {

	private class ValueDebugStringCalculator extends BaseCoreExprVisitor<String> {
		final Environment environment;
		
		public ValueDebugStringCalculator(Environment environment) {
			super();
			this.environment = environment;
		}

		@Override
		public String fallback() {
		    return "(" + exprSource() + ") == " + value.toString();
		}

		@Override
		public String call(Call n) {
			if(n.target instanceof SlotReference) {
				final SlotReference methodReceiver = (SlotReference)n.target;
				final Object lhs = environment.eval(methodReceiver.object);
				return n.getBinaryOperator().map(x -> {
					final Object rhs = environment.eval(n.args.head());
					return "(" + n.toSource() + ") == ("+lhs+" "+x.getOp()+" "+rhs+")"+ " --> " + value;
				}).orSome(P.lazy(() -> {
					return "(" + n.toSource() + ") == ("+lhs+"."+methodReceiver.slotName+"("+ListUtil.insertCommas(n.args.toStream().map(environment::eval))+")" + "==" + value;
				}));
			}
		    return fallback();
		}

		@Override
		public String let(Let let) {
			List<P2<String, FreeExpression>> bindings = let.bindings
					.map(P2.map2_(e -> e.acceptVisitor(FreeExpressionFactory.INSTANCE)))
					.map(P2.map1_(e -> e.id));
			Environment innerEnvironment = new LetEnvironment(environment, bindings);
			return let.body.acceptVisitor(new ValueDebugStringCalculator(innerEnvironment));
		}
	}

	final static ProjectEnvironment environment = ProjectEnvironment.forSourceFile("(test)");

	CoreExpr expr;
	Value value;

	Value intermediateValue;

	public FreeExpression freeExpr;

	public abstract CoreExpr getAst();
	
	@Test
	public void noParseErrors() {
		this.expr = getAst();
    	assertEquals(List.nil(), CoreErrorGatherer.problems(this.expr));
	}

	@Test
    public void evaluates() {
		noParseErrors();
		freeExpr = FreeExpressionFactory.apply(expr);
		intermediateValue = environment.bind(freeExpr);
		Value tmp;
    	try {
			tmp = intermediateValue.force();
    	} catch (Fail f) {
    		tmp = f;
        } catch (Throwable e) {
        	tmp = new Fail(e);
        }
    	this.value = tmp;
    	assertNotNull(value);
    	if(value instanceof Error)
    		throw (Error)value;
    	assertTrue(value.isDefined());
    }

	@Test
	public void printable() {
		evaluates();
		value.toString();
	}

	public String exprSource() {
		return expr.toSource();
	}
	
	@Test
    public void isTruthy() throws Throwable {
    	evaluates();
    	final String valueStr = expr.acceptVisitor(new ValueDebugStringCalculator(environment));
    	assertTrue(valueStr, value.isTruthy());
    }

}