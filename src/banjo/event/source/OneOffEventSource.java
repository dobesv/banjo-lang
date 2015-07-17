package banjo.event.source;

import banjo.event.Event;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.P;
import fj.P2;
import fj.data.List;

public class OneOffEventSource implements EventSource, Value {
	public final Event event;

	public OneOffEventSource(Event event) {
		super();
		this.event = event;
	}

	@Override
	public long nextPollTime(long lastPollTime) {
		return event.timestamp;
	}

	@Override
	public P2<EventSource, List<Event>> poll(long timestamp) {
		if(timestamp >= event.timestamp) {
			return P.p(NilEventSource.INSTANCE, List.single(event));
		}
		return P.p(this, List.nil());
	}

	@Override
	public Reaction<Value> react(Event event) {
		return event.reactE(event).map(this::update);
	}
	
	public OneOffEventSource update(Event newEvent) {
		if(event == newEvent)
			return this;
		return new OneOffEventSource(newEvent);
	}
}
