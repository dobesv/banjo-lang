package banjo.expr.free;

import banjo.eval.expr.SlotInstance;
import banjo.eval.value.Value;

/**
 * A "Free" slot instance doesn't depend on the object, just
 * the environment.  So this just returns the same value every
 * time.
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

}
