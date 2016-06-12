package banjo.eval.expr.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.nio.file.Paths;

import org.junit.Assert;
import org.junit.Test;

import banjo.eval.resolver.GlobalValueResolver;
import banjo.eval.resolver.InstanceAlgebra;
import banjo.eval.resolver.Resolver;
import banjo.eval.resolver.ValueInstanceAlgebra;
import banjo.expr.core.BaseCoreExprVisitor;
import banjo.expr.core.Call;
import banjo.expr.core.CoreErrorGatherer;
import banjo.expr.core.CoreExpr;
import banjo.expr.core.Let;
import banjo.expr.core.Projection;
import banjo.expr.free.FreeExpression;
import banjo.expr.free.FreeExpressionFactory;
import banjo.expr.free.PartialResolver;
import banjo.expr.util.ListUtil;
import banjo.expr.util.SourceFileRange;
import banjo.value.BaseValueVisitor;
import banjo.value.Value;
import banjo.value.fail.Fail;
import banjo.value.fail.FailWithException;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.Set;
import fj.data.TreeMap;

public abstract class BaseExprTest {

    private class ValueDebugStringCalculator<T> extends BaseCoreExprVisitor<String> {
        final Resolver<T> resolver;
        final InstanceAlgebra<T> algebra;
		
        public ValueDebugStringCalculator(Resolver<T> resolver, InstanceAlgebra<T> algebra) {
			super();
            this.resolver = resolver;
            this.algebra = algebra;
		}

		@Override
		public String fallback() {
		    return "(" + exprSource() + ") == " + value.toString();
		}

		@Override
		public String call(Call n) {
			if(n.target instanceof Projection) {
				final Projection methodReceiver = (Projection)n.target;
                final T lhs = resolver.eval(methodReceiver.object, algebra);
				return n.getBinaryOperator().map(x -> {
                    final Object rhs = resolver.eval(n.args.head(), algebra);
					return "(" + n.toSource() + ") == ("+lhs+" "+x.getOp()+" "+rhs+")"+ " --> " + value;
				}).orSome(P.lazy(() -> "(" +
                        n.toSource() +
                        ") == (" +
                        lhs +
                        "." +
                        methodReceiver.projection +
                        "(" +
                    ListUtil.insertCommas(n.args.toStream().map(arg -> resolver.eval(arg, algebra))) +
                        ")" +
                        "==" +
                    value
				));
			}
		    return fallback();
		}

		@Override
		public String let(Let let) {
			List<P2<String, FreeExpression>> bindings = let.bindings
					.map(P2.map2_(e -> e.acceptVisitor(FreeExpressionFactory.INSTANCE)))
					.map(P2.map1_(e -> e.id));
            PartialResolver partialResolver = FreeExpressionFactory.letPartialResolver(TreeMap.treeMap(Ord.stringOrd, bindings));
            FreeExpression freeBody = FreeExpressionFactory.apply(let.body);
            FreeExpression boundBody = freeBody.partial(partialResolver).orSome(freeBody);
            return let.body.acceptVisitor(new ValueDebugStringCalculator<T>(resolver, algebra));
		}
	}

    final static GlobalValueResolver globalResolver = new GlobalValueResolver(FreeExpression.forProjectAtPath(Paths.get("")));

	CoreExpr expr;
	Value value;
	Value intermediateValue;
    FreeExpression freeExpr;

	public abstract CoreExpr getAst();
	
	public void noParseErrors() {
		this.expr = getAst();
    	assertEquals(List.nil(), CoreErrorGatherer.problems(this.expr));
	}

    public void evaluates() {
		noParseErrors();
        calcFreeExpr();
        List<Value> trace = List.nil();
        intermediateValue = freeExpr.eval(trace, globalResolver, ValueInstanceAlgebra.INSTANCE);
        try {
            this.value = intermediateValue.force(trace);
        } catch(Throwable e) {
            this.value = new FailWithException(List.single(intermediateValue), e);
        }
        assertNotNull(value);
        value.acceptVisitor(new BaseValueVisitor<Void>() {

            @Override
            public Void fallback() {
                // No problem
                return null;
            }

            @Override
            public Void failure(Fail failure) {
                StringBuffer sb = new StringBuffer();
                sb.append(failure.toString());
                if(failure.getCause() != null)
                    sb.append("\n Exception: ").append(failure.getCause().toString());
                failure.getTrace(Value.class).forEach(t -> {
                    sb.append("\n at " + t.stackTraceElementString());
                });
                Assert.fail(sb.toString());
                return null;
            }

        });
        assertTrue(value.isDefined(trace));
    }

    @Test
    public void valueToStringWorks() {
		evaluates();
		value.toString();
	}

	public String exprSource() {
		return expr.toSource();
	}
	public Set<SourceFileRange> exprRanges() {
		return expr.getRanges();
	}

	@Test
    public void isTrue() throws Throwable {
    	evaluates();
        final String valueStr = expr.acceptVisitor(new ValueDebugStringCalculator<Value>(globalResolver, ValueInstanceAlgebra.INSTANCE));
        assertTrue(valueStr, value.isTrue(List.single(value)));
    }

    public void calcFreeExpr() {
        if(freeExpr == null) {
            freeExpr = FreeExpressionFactory.apply(expr);
        }
    }

}