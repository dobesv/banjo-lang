package banjo.value;

import static java.util.Objects.requireNonNull;

import banjo.expr.util.SourceFileRange;
import fj.data.List;
import fj.data.Set;

/**
 * Lazy slot reader, used by "extend" as the fallback value.
 */
public class SlotValue extends CalculatedValue {
	public final Value object;
	public final Value originalObject;
	public final Value fallback;
	public final String slotName;
    public final Set<SourceFileRange> ranges;
	
    /**
     * Reference a slot of an object
     * 
     * @param object
     *            Object we are fetching the slot from
     * @param originalObject
     *            Object the code requesting the slot was pulling slot from, before applying any extend operations
     * @param slotName
     *            Name of the slot
     * @param ranges
     *            Source location we can refer to for this slot lookup
     * @param fallback
     *            Value to use if the slot is not defined on <code>object</code>
     */
    public SlotValue(Value object, Value originalObject, String slotName, Set<SourceFileRange> ranges, Value fallback) {
		super();
        this.object = requireNonNull(object);
        this.originalObject = requireNonNull(originalObject);
        this.slotName = requireNonNull(slotName);
        this.ranges = requireNonNull(ranges);
		this.fallback = fallback;
	}
	
    /**
     * Reference a slot of an object
     * 
     * @param object
     *            Object whose slot we wish to read
     * @param ranges
     *            Source location we can refer to for this slot lookup
     * @param slotName
     *            Name of the slot
     */
    public SlotValue(Value object, Set<SourceFileRange> ranges, String slotName) {
        this(object, object, slotName, ranges, null);
	}

	@Override
	public Value calculate(List<Value> trace) {
        Value v = object.slot(trace.cons(this), originalObject, slotName, ranges, fallback);
		if(this == v) throw new Error();
		return v;
	}
	
	public SlotValue update(Value newObject, Value newSelf, Value newFallback) {
		if(newObject == this.object && newSelf == this.originalObject && newFallback == this.fallback)
			return this;
        return new SlotValue(newObject, newSelf, slotName, ranges, newFallback);
	}

}