package banjo.value;

import banjo.event.Event;
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
	private ObservableSlotValue observable;
	
	public SlotValue(Value object, Value self, String slotName, Value fallback) {
		super();
		this.object = object;
		this.self = self;
		this.slotName = slotName;
		this.fallback = fallback;
	}
	
	public SlotValue(Value object, String slotName) {
		this(object, object, slotName, null);
	}

	@Override
	public Value calculate() {
		Value v = object.slot(self, slotName, fallback);
		if(this == v) throw new Error();
		return v;
	}
	
	@Override
	public Reaction<Value> calculationReact(Event event) {
		return Reaction.to(object, self, fallback, event).map(p -> this.update(p._1(), p._2(), p._3()));
	}
	
	@Override
	public boolean isCalculationReactive() {
		return object.isReactive();
	}
	
	public SlotValue update(Value newObject, Value newSelf, Value newFallback) {
		if(newObject == this.object && newSelf == this.self && newFallback == this.fallback)
			return this;
		return new SlotValue(newObject, newSelf, slotName, newFallback);
	}

	@Override
	public ObservableValue<Value> toObservableValue() {
		if(observable == null)
			observable = new ObservableSlotValue(this);
		return observable;
	}
}