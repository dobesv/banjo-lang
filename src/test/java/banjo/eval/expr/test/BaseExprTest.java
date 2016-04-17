package banjo.eval.expr.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Assert;
import org.junit.Test;

import banjo.eval.Fail;
import banjo.eval.FailWithException;
import banjo.eval.environment.Environment;
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
import fj.data.Set;

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
	
	public void noParseErrors() {
		this.expr = getAst();
    	assertEquals(List.nil(), CoreErrorGatherer.problems(this.expr));
	}

    public void evaluates() {
		noParseErrors();
        Set<SourceFileRange> ranges = exprRanges();

        freeExpr = FreeExpressionFactory.apply(expr);
        List<Value> trace = List.nil();
        intermediateValue = freeExpr.apply(environment, trace);
        Value tmp;
        try {
            this.value = intermediateValue.force(trace);
        } catch(Throwable e) {
            this.value = new FailWithException(List.single(intermediateValue), e);
        }
        assertNotNull(value);
        if(value instanceof Fail) {
            Fail failure = (Fail) value;
            System.err.println(value.toString());
            if(failure.getCause() != null)
                System.err.println("  Exception: " + failure.getCause().toString());
            failure.getTrace().forEach(t -> {
                System.err.println("  at " + t.stackTraceElementString());
            });
            Assert.fail(failure.toString());
        }
        assertTrue(value.isDefined(trace));
    }

	public void printable() {
		evaluates();
		value.toString();
	}

	public String exprSource() {
		return expr.toSource();
	}
	public Set<SourceFileRange> exprRanges() {
		return expr.getSourceFileRanges();
	}

	@Test
    public void isTruthy() throws Throwable {
    	evaluates();
    	final String valueStr = expr.acceptVisitor(new ValueDebugStringCalculator(environment));
        assertTrue(valueStr, value.isTruthy(List.single(value)));
    }

}