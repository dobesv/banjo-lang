package banjo.eval.expr.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.function.Supplier;

import org.junit.Test;

import banjo.eval.Fail;
import banjo.eval.environment.Environment;
import banjo.eval.util.JavaRuntimeSupport;
import banjo.expr.core.BaseCoreExprVisitor;
import banjo.expr.core.Call;
import banjo.expr.core.CoreErrorGatherer;
import banjo.expr.core.CoreExpr;
import banjo.expr.core.Let;
import banjo.expr.core.Projection;
import banjo.expr.free.FreeExpression;
import banjo.expr.free.FreeExpressionFactory;
import banjo.expr.util.ListUtil;
import banjo.expr.util.SourceFileRange;
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
			if(n.target instanceof Projection) {
				final Projection methodReceiver = (Projection)n.target;
				final Value lhs = environment.eval(methodReceiver.object);
				return n.getBinaryOperator().map(x -> {
					final Object rhs = environment.eval(n.args.head());
					return "(" + n.toSource() + ") == ("+lhs+" "+x.getOp()+" "+rhs+")"+ " --> " + value;
				}).orSome(P.lazy(() -> {
					return "(" + n.toSource() + ") == ("+lhs+"."+methodReceiver.projection+"("+ListUtil.insertCommas(n.args.toStream().map(environment::eval))+")" + "==" + value;
				}));
			}
		    return fallback();
		}

		@Override
		public String let(Let let) {
			List<P2<String, FreeExpression>> bindings = let.bindings
					.map(P2.map2_(e -> e.acceptVisitor(FreeExpressionFactory.INSTANCE)))
					.map(P2.map1_(e -> e.id));
			Environment innerEnvironment = environment.let(bindings);
			return let.body.acceptVisitor(new ValueDebugStringCalculator(innerEnvironment));
		}
	}

	final static Environment environment = Environment.forCurrentDirectory();

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
		List<Supplier<StackTraceElement>> oldStack = JavaRuntimeSupport.stack.get();
		try {
			List<SourceFileRange> ranges = exprRanges();
			if(ranges.isNotEmpty()) {
				JavaRuntimeSupport.stack.set(List.single(()->new StackTraceElement("tests", exprSource(), ranges.head().getSourceFile().toString(), ranges.head().getStartLine())));
			}
			
			freeExpr = FreeExpressionFactory.apply(expr);
			intermediateValue = environment.eval(freeExpr);
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
		} finally {
			JavaRuntimeSupport.stack.set(oldStack);
		}
    }

	@Test
	public void printable() {
		evaluates();
		value.toString();
	}

	public String exprSource() {
		return expr.toSource();
	}
	public List<SourceFileRange> exprRanges() {
		return expr.getSourceFileRanges();
	}

	@Test
    public void isTruthy() throws Throwable {
    	evaluates();
    	final String valueStr = expr.acceptVisitor(new ValueDebugStringCalculator(environment));
    	assertTrue(valueStr, value.isTruthy());
    }

}