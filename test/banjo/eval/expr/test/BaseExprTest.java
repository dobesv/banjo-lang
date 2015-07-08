package banjo.eval.expr.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import banjo.eval.Fail;
import banjo.eval.expr.ProjectEnvironment;
import banjo.eval.util.JavaRuntimeSupport;
import banjo.eval.value.Value;
import banjo.expr.core.BaseCoreExprVisitor;
import banjo.expr.core.Call;
import banjo.expr.core.CoreErrorGatherer;
import banjo.expr.core.CoreExpr;
import banjo.expr.core.SlotReference;
import banjo.expr.free.FreeExpression;
import banjo.expr.free.FreeExpressionFactory;
import banjo.expr.util.ListUtil;
import fj.P;
import fj.data.List;

public abstract class BaseExprTest {

	final static ProjectEnvironment environment = ProjectEnvironment.forSourceFile("(test)");

	final CoreExpr expr;
	final Value value;

	final Value intermediateValue;

	public final FreeExpression freeExpr;

	public BaseExprTest(CoreExpr ast) {
		super();
		this.expr = ast;
		freeExpr = FreeExpressionFactory.apply(ast);
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
	}

	@Test
	public void noParseErrors() {
    	assertEquals(List.nil(), CoreErrorGatherer.problems(this.expr));
	}

	@Test
    public void evaluates() {
    	assertNotNull(value);
    	if(value instanceof Error)
    		throw (Error)value;
    	assertTrue(value.isDefined());
    }

	@Test
	public void printable() {
		value.toString();
	}

	@Test
    public void isTruthy() throws Throwable {
    	evaluates();
    	final String valueStr = expr.acceptVisitor(new BaseCoreExprVisitor<String>() {
    		@Override
    		public String fallback() {
    		    return expr.toSource() + " = " + value.toString();
    		}

    		@Override
    		public String call(Call n) {
    			if(n.target instanceof SlotReference) {
    				final SlotReference methodReceiver = (SlotReference)n.target;
    				final Object lhs = environment.eval(methodReceiver.object);
    				return n.getBinaryOperator().map(x -> {
    					final Object rhs = environment.eval(n.args.head());
    					return "("+lhs+" "+x.getOp()+" "+rhs+")"+ " --> " + value;
    				}).orSome(P.lazy(() -> {
    					return "( == "+lhs+"."+methodReceiver.slotName+"("+ListUtil.insertCommas(n.args.toStream().map(environment::eval))+")" + "-->" + value;
    				}));
    			}
    		    return value.toString();
    		}
    	});
    	assertTrue(valueStr, value.isTruthy());
    }

}