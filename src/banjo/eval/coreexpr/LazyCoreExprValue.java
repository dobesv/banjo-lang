package banjo.eval.coreexpr;

import static java.util.Objects.requireNonNull;

import java.util.function.Supplier;

import banjo.dom.core.CoreExpr;
import banjo.eval.CalculatedValue;
import banjo.eval.util.JavaRuntimeSupport;
import fj.data.List;

public class LazyCoreExprValue extends LazyValue implements CalculatedValue {
	public CoreExprEvaluator evaluator;
	public CoreExpr expr;
	public LazyCoreExprValue(CoreExpr expr, CoreExprEvaluator evaluator, List<Supplier<StackTraceElement>> stack) {
	    super(stack);
	    this.expr = requireNonNull(expr);
	    this.evaluator = requireNonNull(evaluator);
	    this.stack = requireNonNull(stack);
    }

	public LazyCoreExprValue(CoreExpr expr) {
	    super(List.nil());
	    this.expr = requireNonNull(expr);
	    this.evaluator = null;
    }

    @Override
	public Object calculate() {
		if(result == null && evaluator != null) {
			List<Supplier<StackTraceElement>> oldStack = JavaRuntimeSupport.stack.get();
			try {
				JavaRuntimeSupport.stack.set(stack);
				CoreExprEvaluator tmpEvaluator = evaluator;
				CoreExpr tmpExpr = expr;
				evaluator = null;
				result = tmpEvaluator.evaluate(tmpExpr);
				expr = null;
				stack = null;
			} finally {
				JavaRuntimeSupport.stack.set(oldStack);
			}
			while(result != this && result instanceof LazyCoreExprValue) {
				result = ((LazyCoreExprValue)result).calculate();
			}
		}
	    return result;
	}

	@Override
	public String toString() {
		try {
			return super.toString();
		} catch(Throwable t) {
			if(result == null && expr != null) {
				return "lazy("+expr+")";
			} else {
				return "lazy(<cannot make string>)";
			}
		}
	}


}
