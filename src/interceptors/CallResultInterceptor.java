package interceptors;

import banjo.dom.source.Operator;
import banjo.eval.Value;
import banjo.eval.util.JavaRuntimeSupport;
import fj.data.List;

/**
 * Implement function composition.  The first function is called with with the
 * arguments to this function.  The result of calling first is passed to second.
 * The result of second is the result of the function.
 */
public class CallResultInterceptor extends Interceptor {

	public CallResultInterceptor(Object second, Object first) {
	    super(second, first);
    }

	@Override
	public Object call(Object recurse, Object baseImpl, List<Object> arguments) {
		Object intermediateValue = JavaRuntimeSupport.call(target, arguments);
		Object finalValue = JavaRuntimeSupport.call(interceptor, List.single(intermediateValue));
		return finalValue;
	}

	@Override
	protected String toStringFallback() {
	    return target + " " + Operator.FUNCTION_COMPOSITION_RIGHT.getOp() + " " + interceptor;
	}
}
