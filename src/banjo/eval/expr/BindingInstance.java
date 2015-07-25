package banjo.eval.expr;

import banjo.event.Event;
import banjo.value.Reaction;
import banjo.value.Reactive;
import banjo.value.Value;
import fj.P3;

public class BindingInstance implements Reactive<BindingInstance> {
	public final Value value;
	public final String slotName;
	public final Value baseSlotValue;
	public final Value baseFunction;

	/**
	 * A binding specifies the value and other attributes associated with a name
	 * in (lexical) scope.
	 *
	 * When a binding is a "self" or "recursion" binding, we need some extra information
	 * to facilitate calls to get the "previous" value or implementation.
	 *
	 * @param value The plain value associated with the name
	 * @param slotName When evaluating a slot's value, this is the name of the slot we are evaluating relative to the
	 *   object value.
	 * @param baseSlotValue When evaluating a slot's value, this is the value of the same slot we are
	 *   evaluating, but in the object's next "base"
	 * @param baseFunction In the self-referential binding of a function, if the function is an extension
	 *   of another function, the previous/original implementation is available here.
	 */
	public BindingInstance(Value value, String slotName, Value baseSlotValue, Value baseFunction) {
        super();
        this.value = value;
        this.slotName = slotName;
        this.baseSlotValue = baseSlotValue;
        this.baseFunction = baseFunction;
    }

	public static BindingInstance let(Value value) {
		return new BindingInstance(value, null, null, null);
	}

	public static BindingInstance functionSelf(Value sourceObject, Value baseFunction) {
		return new BindingInstance(sourceObject, null, null, baseFunction);
	}

	public static BindingInstance slotSourceObject(Value sourceObject, String slotName, Value baseSlotValue) {
		return new BindingInstance(sourceObject, slotName, baseSlotValue, null);
	}

	@Override
	public String toString() {
	    return String.valueOf(value);
	}

	public boolean bindsSelfForSlot(String id) {
	    return id.equals(slotName);
    }

	public Reaction<BindingInstance> react(Event event) {
		return Reaction.to(value, baseSlotValue, baseFunction, event).map(this::update);
	}

	@Override
	public boolean isReactive() {
		return value.isReactive() || (baseSlotValue != null && baseSlotValue.isReactive()) || (baseFunction != null && baseFunction.isReactive());
	}
	
	public BindingInstance update(P3<Value,Value,Value> values) {
		return update(values._1(), values._2(), values._3());
	}
	public BindingInstance update(Value newValue, Value newBaseSlotValue, Value newBaseFunction) {
		return (value == newValue && baseSlotValue == newBaseSlotValue && baseFunction == newBaseFunction) ? this :
			new BindingInstance(newValue, slotName, newBaseSlotValue, newBaseFunction);
	}
}