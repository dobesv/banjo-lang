package banjo.eval.expr.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.nio.file.Paths;

import org.junit.Assert;
import org.junit.Test;

import banjo.eval.resolver.GlobalValueResolver;
import banjo.eval.resolver.ValueInstanceAlgebra;
import banjo.expr.core.CoreErrorGatherer;
import banjo.expr.core.CoreExpr;
import banjo.expr.free.FreeExpression;
import banjo.expr.free.FreeExpressionFactory;
import banjo.expr.util.SourceFileRange;
import banjo.value.BaseValueVisitor;
import banjo.value.Value;
import banjo.value.fail.Fail;
import banjo.value.fail.FailWithException;
import fj.data.List;
import fj.data.Set;

public abstract class BaseExprTest {

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
        final String valueStr = expr.acceptVisitor(new ValueDebugStringCalculator<Value>(globalResolver, ValueInstanceAlgebra.INSTANCE, expr, value));
        assertTrue(valueStr, value.isTrue(List.single(value)));
    }

    public void calcFreeExpr() {
        if(freeExpr == null) {
            freeExpr = FreeExpressionFactory.apply(expr);
        }
    }

}