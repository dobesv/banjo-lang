package banjo.eval;

import banjo.dom.source.Operator;
import banjo.eval.util.JavaRuntimeSupport;
import fj.data.List;

/**
 * Implement function composition.  The first function is called with with the
 * arguments to this function.  The result of calling first is passed to second.
 * The result of second is the result of the function.
 */
public class FunctionComposition extends Value {
	public final Object first;
	public final Object second;

	public FunctionComposition(Object first, Object second) {
	    super();
	    this.first = first;
	    this.second = second;
    }

	@Override
	public Object call(Object recurse, Object baseImpl, List<Object> arguments) {
		Object intermediateValue = JavaRuntimeSupport.call(first, arguments);
		Object finalValue = JavaRuntimeSupport.call(second, List.single(intermediateValue));
		return finalValue;
	}

	@Override
	public String toString() {
	    return "(" + first + " " + Operator.FUNCTION_COMPOSITION_LEFT.getOp() + " " + second + ")";
	}
}
