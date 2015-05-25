package banjo.expr.free;

import java.util.function.Supplier;

import banjo.eval.coreexpr.SlotInstance;

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
