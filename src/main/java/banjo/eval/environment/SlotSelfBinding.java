package banjo.eval.environment;

import banjo.value.Value;

public class SlotSelfBinding implements Binding {

	public final Value sourceObject;
	public final String slotName;

	public SlotSelfBinding(Value sourceObject, String slotName) {
		this.sourceObject = sourceObject;
		this.slotName = slotName;
	}

	public SlotSelfBinding update(Value sourceObject) {
		if(sourceObject == this.sourceObject)
			return this;
		return new SlotSelfBinding(sourceObject, slotName);
	}
	
	@Override
	public <T> T acceptVisitor(BindingVisitor<T> visitor) {
		return visitor.slot(sourceObject, slotName);
	}

}
