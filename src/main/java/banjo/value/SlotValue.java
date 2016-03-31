package banjo.value;

import static java.util.Objects.requireNonNull;

import banjo.event.PastEvent;
import banjo.expr.util.SourceFileRange;
import fj.data.Set;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;

/**
 * Lazy slot reader, used by "extend" as the fallback value.
 */
public class SlotValue extends CalculatedValue {
	public static final class ObservableSlotValue extends ObjectBinding<Value> {
		final ObservableValue<Value> objectBinding;
		final ObservableValue<Value> selfBinding;
		final ObservableValue<Value> fallbackBinding;
		SlotValue currentValue;

		public ObservableSlotValue(SlotValue slotValue) {
			this.currentValue = slotValue;
			objectBinding = slotValue.object.toObservableValue();
			selfBinding = slotValue.self.toObservableValue();
			fallbackBinding = slotValue.fallback.toObservableValue();
			bind(objectBinding, selfBinding, fallbackBinding);
		}

		@Override
		protected Value computeValue() {
			return currentValue = currentValue.update(objectBinding.getValue(), selfBinding.getValue(), fallbackBinding.getValue());
		}
	}

	public final Value object;
	public final Value self;
	public final Value fallback;
	public final String slotName;
    public final Set<SourceFileRange> ranges;
    private ObservableSlotValue observable;
	
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
	public Value calculate() {
		Value v = object.slot(self, slotName, ranges, fallback);
		if(this == v) throw new Error();
		return v;
	}
	
	@Override
	public Reaction<Value> calculationReact(PastEvent event) {
		return Reaction.to(object, self, fallback, event).map(p -> this.update(p._1(), p._2(), p._3()));
	}
	
	@Override
	public boolean isCalculationReactive() {
        return object.isReactive() || self.isReactive() || (fallback != null && fallback.isReactive());
	}
	
	public SlotValue update(Value newObject, Value newSelf, Value newFallback) {
		if(newObject == this.object && newSelf == this.self && newFallback == this.fallback)
			return this;
        return new SlotValue(newObject, newSelf, slotName, ranges, newFallback);
	}

	@Override
	public ObservableValue<Value> toObservableValue() {
		if(observable == null)
			observable = new ObservableSlotValue(this);
		return observable;
	}
}