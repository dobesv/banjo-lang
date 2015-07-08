package banjo.eval.expr;

import banjo.eval.value.Value;

public class BindingInstance {
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
}