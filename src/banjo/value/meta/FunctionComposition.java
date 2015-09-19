package banjo.value.meta;

import banjo.event.Event;
import banjo.expr.source.Operator;
import banjo.value.FunctionTrait;
import banjo.value.Reaction;
import banjo.value.Value;
import banjo.value.meta.SlotMapper.ObservableSlotMapper;
import fj.P2;
import fj.data.List;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;

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
	public Value call(Value recurse, Value baseImpl, List<Value> arguments) {
		return this.call(arguments);
	}
	
	@Override
	public Value call(List<Value> arguments) {
		Value intermediateValue = first.call(arguments);
		Value finalValue = second.call(List.single(intermediateValue));
		return finalValue;
	}

	@Override
    public String toStringFallback() {
	    return second + " " + Operator.FUNCTION_COMPOSITION_LEFT.getOp() + " " + first;
	}

	@Override
	public Reaction<Value> react(Event event) {
		return Reaction.to(first, second, event).map(P2.tuple(this::update));
	}

	@Override
	public boolean isReactive() {
		return first.isReactive() || second.isReactive();
	}
	
	public FunctionComposition update(Value newFirst, Value newSecond) {
		return (first == newFirst && second == newSecond) ? this : new FunctionComposition(newSecond, newFirst);
	}

	@Override
	public boolean isDefined() {
		return first.isDefined() && second.isDefined();
	}

	public static final class ObservableFunctionComposition extends ObjectBinding<Value> {
		final ObservableValue<Value> firstBinding;
		final ObservableValue<Value> secondBinding;
		FunctionComposition functionComposition;
		public ObservableFunctionComposition(FunctionComposition functionComposition) {
			super();
			firstBinding = functionComposition.first.toObservableValue();
			secondBinding = functionComposition.second.toObservableValue();
			bind(firstBinding, secondBinding);
			this.functionComposition = functionComposition;
		}
		
		@Override
		public void dispose() {
			unbind(firstBinding, secondBinding);
		}
		
		@Override
		protected Value computeValue() {
			return functionComposition = functionComposition.update(firstBinding.getValue(), secondBinding.getValue());
		}
		
		
	}

	@Override
	public ObservableValue<Value> toObservableValue() {
		return new ObservableFunctionComposition(this);
	}
	
}
