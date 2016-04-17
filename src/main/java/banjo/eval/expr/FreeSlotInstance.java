package banjo.eval.expr;

import banjo.value.Value;
import fj.data.List;

/**
 * A "Free" slot instance doesn't depend on the object, just
 * the environment.  So this just returns the same value every
 * time because we've already bound to the environment.
 */
public class FreeSlotInstance implements SlotInstance {

	public final Value value;

	public FreeSlotInstance(Value value) {
	    super();
	    this.value = value;
    }

	@Override
    public Value apply(List<Value> trace, Value t, Value u) {
	    return value;
	}
	
	private FreeSlotInstance update(Value newValue) {
		return (value == newValue)? this : new FreeSlotInstance(newValue);
	}

	public SlotInstance withValue(Value newValue) {
		if(newValue == this.value)
			return this;
		return new FreeSlotInstance(newValue);
	}
	
	@Override
	public String toString() {
		return value.toString();
	}
}
