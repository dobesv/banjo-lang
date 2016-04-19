package banjo.eval.environment;

import banjo.value.Value;

public interface BindingVisitor<T> {

	T let(Value value);

    T functionSelf(Value function);

	T functionSelfWithBase(Value function, Value baseFunction);

    T slot(Value sourceObject, String slotName);

    T slotWithBase(Value sourceObject, String slotName, Value baseSlotValue);
}
