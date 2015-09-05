package banjo.value.meta;

import banjo.event.Event;
import banjo.value.BaseInertValue;
import banjo.value.Reaction;
import banjo.value.Value;

public class SlotNames extends BaseInertValue implements Value {

	public static final Value INSTANCE = new SlotNames();

	@Override
	public Reaction<Value> react(Event event) {
		return Reaction.of(this);
	}
	
	@Override
	public Value slot(Value self, String name, Value fallback) {
		return Value.fromJava(name);
	}
	
	@Override
	public Value slot(String name) {
		return Value.fromJava(name);
	}
}
