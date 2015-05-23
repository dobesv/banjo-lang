package banjo.eval.coreexpr;

import java.util.function.Supplier;

public class FreeSlotInstance implements SlotInstance {

	public final Supplier<Object> value;

	public FreeSlotInstance(Supplier<Object> value) {
	    super();
	    this.value = value;
    }

	@Override
	public Supplier<Object> apply(Object t, Object u) {
	    return value;
	}

}
