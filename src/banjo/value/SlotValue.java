package banjo.value;

import banjo.event.Event;

/**
 * Lazy slot reader, used by "extend" as the fallback value.
 */
public class SlotValue extends CalculatedValue {
	public final Value object;
	public final Value self;
	public final Value fallback;
	public final String slotName;
	
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
	
	public Value update(Value newObject, Value newSelf, Value newFallback) {
		if(newObject == this.object && newSelf == this.self && newFallback == this.fallback)
			return this;
		return new SlotValue(newObject, newSelf, slotName, newFallback);
	}

}