package banjo.value;

import banjo.eval.util.SlotName;
import banjo.event.Event;
import javafx.beans.value.ObservableValue;

public class TimeValue implements Value {
	final long timestamp;
	final long period;
	
	/**
	 * Create a time value with a given time and maximum time between polls
	 * 
	 * @param timestamp Time of last update
	 * @param period Maximum time between updates
	 */
	public TimeValue(long timestamp, long period) {
		super();
		this.timestamp = timestamp;
		this.period = period;
	}

	@Override
	public Reaction<Value> react(Event event) {
		long newTimestamp = event.timestamp;
		return new Reaction<Value>(update(newTimestamp), newTimestamp+period);
	}

	public Value update(long newTimestamp) {
		if(newTimestamp == timestamp)
			return this;
		return new TimeValue(newTimestamp, period);
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
	
	@Override
	public boolean isReactive() {
		return true;
	}

	@Override
	public ObservableValue<Value> toObservableValue() {
		return new ObservableReactive<Value>(this);
	}
}
