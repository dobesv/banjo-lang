package banjo.eval.environment;

import banjo.value.Value;

public interface BindingVisitor<T> {

	T let(Value value);
	T functionRecursive(Value function);
	T functionRecursiveWithBase(Value function, Value baseFunction);
	T slot(Value sourceObject, String slotName);
	T slotWithBase(Value sourceObject, String slotName, Value baseSlotValue);
}
