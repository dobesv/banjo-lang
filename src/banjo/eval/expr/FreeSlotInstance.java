package banjo.eval.expr;

import banjo.event.Event;
import banjo.value.Reaction;
import banjo.value.Value;

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
	public Value apply(Value t, Value u) {
	    return value;
	}
	
	@Override
	public Reaction<SlotInstance> react(Event event) {
		return value.react(event).map(this::update);
	}

	@Override
	public boolean isReactive() {
		return value.isReactive();
	}
	
	private SlotInstance update(Value newValue) {
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
