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
	public final Value self;
	public final Value fallback;
	public final String slotName;
    public final Set<SourceFileRange> ranges;
	
    public SlotValue(Value object, Value self, String slotName, Set<SourceFileRange> ranges, Value fallback) {
		super();
        this.object = requireNonNull(object);
        this.self = requireNonNull(self);
        this.slotName = requireNonNull(slotName);
        this.ranges = requireNonNull(ranges);
		this.fallback = fallback;
	}
	
    public SlotValue(Value object, String slotName, Set<SourceFileRange> ranges) {
        this(object, object, slotName, ranges, null);
	}

	@Override
	public Value calculate(List<Value> trace) {
		Value v = object.slot(trace, self, slotName, ranges, fallback);
		if(this == v) throw new Error();
		return v;
	}
	
	public SlotValue update(Value newObject, Value newSelf, Value newFallback) {
		if(newObject == this.object && newSelf == this.self && newFallback == this.fallback)
			return this;
        return new SlotValue(newObject, newSelf, slotName, ranges, newFallback);
	}

}