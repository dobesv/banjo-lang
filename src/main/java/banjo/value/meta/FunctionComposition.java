package banjo.value.meta;

import banjo.expr.source.Operator;
import banjo.value.FunctionTrait;
import banjo.value.Value;
import banjo.value.ValueVisitor;
import fj.data.List;

/**
 * Implement function composition.  The first function is called with with the
 * arguments to this function.  The result of calling first is passed to second.
 * The result of second is the result of the composed function.
 */
public class FunctionComposition extends FunctionTrait implements Value {
	final Value first;
	final Value second;
	public FunctionComposition(Value second, Value first) {
		this.first = first;
		this.second = second;
    }

	@Override
	public Value call(List<Value> trace, Value recurse, Value baseImpl, List<Value> arguments) {
		return this.call(trace, arguments);
	}
	
	@Override
	public Value call(List<Value> trace, List<Value> arguments) {
		Value intermediateValue = first.call(trace, arguments);
		Value finalValue = second.call(trace, List.single(intermediateValue));
		return finalValue;
	}

	@Override
    public String toStringFallback(List<Value> trace) {
	    return second + " " + Operator.FUNCTION_COMPOSITION_LEFT.getOp() + " " + first;
	}
	
	public FunctionComposition update(Value newFirst, Value newSecond) {
		return (first == newFirst && second == newSecond) ? this : new FunctionComposition(newSecond, newFirst);
	}

	@Override
	public boolean isDefined(List<Value> trace) {
		return first.isDefined(trace) && second.isDefined(trace);
	}
	
    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.functionComposition(this);
    }
}
