package banjo.value;

import banjo.event.Event;

/**
 * An object with a single slot - which is the base slot value
 * for a field.  Used to implement "base" or "super" slot
 * projections.
 */
public class ObjectBaseValue implements Value {
	public final String selfName;
	public final String slotName;
	public final Value slotValue;
	
	public ObjectBaseValue(String selfName, String slotName, Value slotValue) {
		this.selfName = selfName;
		this.slotName = slotName;
		this.slotValue = slotValue;
	}
	
	@Override
	public Value slot(String name) {
		if(name.equals(slotName))
			return slotValue;
		return Value.super.slot(name);
	}
	
	@Override
	public Value slot(Value self, String name, Value fallback) {
		if(name.equals(slotName))
			return slotValue;
		return Value.super.slot(self, name, fallback);
	}

	@Override
	public boolean isReactive() {
		return slotValue.isReactive();
	}
	
	@Override
	public Reaction<Value> react(Event event) {
		return slotValue.react(event).map(this::update);
	}
	
	public Value update(Value newSlotValue) {
		if(newSlotValue == slotValue)
			return this;
		return new ObjectBaseValue(selfName, slotName, newSlotValue);
	}
	
	@Override
	public String toString() {
		return selfName+":"+slotName;
	}

}
