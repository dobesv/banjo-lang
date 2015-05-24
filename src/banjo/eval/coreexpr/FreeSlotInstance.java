package banjo.eval.coreexpr;

import java.util.function.Supplier;

public class FreeSlotInstance implements SlotInstance {

	public final Object value;

	public FreeSlotInstance(Object value) {
	    super();
	    this.value = value;
    }

	@Override
	public Object apply(Object t, Object u) {
	    return value;
	}

}
