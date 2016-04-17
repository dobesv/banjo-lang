package banjo.value;

import banjo.expr.util.SourceFileRange;
import fj.data.List;
import fj.data.Set;

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
	public Value slot(List<Value> trace, String name, Set<SourceFileRange> ranges) {
		if(name.equals(slotName))
			return slotValue;
		return Value.super.slot(trace, name, ranges);
	}
	
	@Override
	public Value slot(List<Value> trace, Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
		if(name.equals(slotName))
			return slotValue;
		return Value.super.slot(trace, self, name, ranges, fallback);
	}

	public ObjectBaseValue update(Value newSlotValue) {
		if(newSlotValue == slotValue)
			return this;
		return new ObjectBaseValue(selfName, slotName, newSlotValue);
	}
	
	@Override
	public String toString() {
		return selfName+":"+slotName;
	}

    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.objectBaseValue(this);
    }
}
