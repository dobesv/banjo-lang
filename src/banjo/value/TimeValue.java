package banjo.value;

import banjo.eval.util.SlotName;
import banjo.event.Event;

public class TimeValue implements Value {
	final long timestamp;
	
	public TimeValue(long timestamp) {
		super();
		this.timestamp = timestamp;
	}

	@Override
	public Reaction<Value> react(Event event) {
		long newTimestamp = event.timestamp;
		return Reaction.none(update(newTimestamp));
	}

	public Value update(long newTimestamp) {
		if(newTimestamp == timestamp)
			return this;
		return new TimeValue(newTimestamp);
	}

	@SlotName("epoch seconds")
	public long epochSeconds() {
		return ((timestamp + 500L) / 1000L) * 1000L; // Round to nearest second
	}
	
	@SlotName("epoch millis")
	public long epochMillis() {
		return timestamp;
	}
	
	@Override
	public Value slot(Value self, String name, Value fallback) {
		if(name.equals("epoch seconds") || name.equals("epoch millis")) {
			return JavaObjectValue.readJavaObjectSlot(self, fallback, name, this); 
		}
		return Value.super.slot(self, name, fallback);
	}
}
