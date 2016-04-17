package banjo.eval.environment;

import banjo.value.Value;

public class SlotSelfBindingWithBase implements Binding {

	public final Value sourceObject;
	public final String slotName;
	public final Value baseSlotValue;

	public SlotSelfBindingWithBase(Value sourceObject, String slotName, Value baseSlotValue) {
		this.sourceObject = sourceObject;
		this.slotName = slotName;
		this.baseSlotValue = baseSlotValue;
	}

	public SlotSelfBindingWithBase update(Value sourceObject, Value baseSlotValue) {
		if(sourceObject == this.sourceObject && baseSlotValue == this.baseSlotValue)
			return this;
		return new SlotSelfBindingWithBase(sourceObject, slotName, baseSlotValue);
	}

	@Override
	public <T> T acceptVisitor(BindingVisitor<T> visitor) {
		return visitor.slotWithBase(sourceObject, slotName, baseSlotValue);
	}

}
