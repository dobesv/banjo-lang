package banjo.eval.interceptors;

import banjo.eval.value.Value;
import banjo.expr.source.Operator;
import fj.data.List;

/**
 * Implement function composition.  The first function is called with with the
 * arguments to this function.  The result of calling first is passed to second.
 * The result of second is the result of the composed function.
 */
public class CallResultInterceptor extends Interceptor {

	public CallResultInterceptor(Value second, Value first) {
	    super(second, first);
    }

	@Override
	public Value call(Value recurse, Value baseImpl, List<Value> arguments) {
		Value intermediateValue = target.call(arguments);
		Value finalValue = interceptor.call(List.single(intermediateValue));
		return finalValue;
	}

	@Override
    public String toStringFallback() {
	    return target + " " + Operator.FUNCTION_COMPOSITION_RIGHT.getOp() + " " + interceptor;
	}
}
