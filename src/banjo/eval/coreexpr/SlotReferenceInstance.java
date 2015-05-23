package banjo.eval.coreexpr;

import java.util.function.Supplier;

import banjo.eval.util.JavaRuntimeSupport;

public class SlotReferenceInstance implements Supplier<Object> {

	public final SlotInstance slotInstance;
	public final Object sourceObject;
	public final Object prevSlotValue;

	public SlotReferenceInstance(SlotInstance slotInstance, Object sourceObject, Object prevSlotValue) {
		this.slotInstance = slotInstance;
		this.sourceObject = sourceObject;
		this.prevSlotValue = prevSlotValue;
    }

	@Override
	public Object get() {
		return slotInstance.apply(sourceObject, prevSlotValue);
	}

	@Override
	public String toString() {
	    return String.valueOf(JavaRuntimeSupport.force(get()));
	}


}
