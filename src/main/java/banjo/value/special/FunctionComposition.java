package banjo.value.special;

import banjo.eval.EvalContext;
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
	public FunctionComposition(Value second, Value first, Value functionTrait) {
        super(functionTrait);
		this.first = first;
		this.second = second;
    }

	@Override
	public Value call(EvalContext<Value> ctx, Value recurse, Value baseImpl, List<Value> arguments) {
		return this.call(ctx, arguments);
	}
	
	@Override
	public Value call(EvalContext<Value> ctx, List<Value> arguments) {
		Value intermediateValue = first.call(ctx, arguments);
		Value finalValue = second.call(ctx, List.single(intermediateValue));
		return finalValue;
	}

	@Override
    public String toStringFallback(EvalContext<Value> ctx) {
	    return second + " " + Operator.FUNCTION_COMPOSITION_LEFT.getOp() + " " + first;
	}
	
	public FunctionComposition update(Value newFirst, Value newSecond) {
        return (first == newFirst && second == newSecond) ? this
                : new FunctionComposition(newSecond, newFirst, this.trait);
	}

	@Override
	public boolean isDefined(EvalContext<Value> ctx) {
		return first.isDefined(ctx) && second.isDefined(ctx);
	}
	
    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.functionComposition(this);
    }
}
