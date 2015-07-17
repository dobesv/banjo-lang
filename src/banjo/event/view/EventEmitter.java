package banjo.event.view;

import banjo.event.Event;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.P2;
import fj.data.List;

/**
 * For each value we get from the event view, emit an event with that
 * value and a new event variant.
 * 
 * The assumption here is that the event has been filtered and mapped to
 * some value of interested to some other party.
 */
public class EventEmitter implements Value {
	public final EventView eventView;
	public final String variant;

	public EventEmitter(EventView eventView, String variant) {
		super();
		this.eventView = eventView;
		this.variant = variant;
	}

	@Override
	public Reaction<Value> react(Event event) {
		Reaction<EventView> r = Reaction.to(eventView, event);
		P2<EventView, List<Value>> p = r.v.apply(event);
		List<Value> newEvents = p._2();
		EventView newEventView = p._1();
		List<Event> eventsToEmit = newEvents.map(evt -> new Event(event.timestamp, variant, evt));
		return r.from(this.update(newEventView)).withEvents(eventsToEmit);
	}
	
	public Value update(EventView v) {
		if(v == eventView)
			return this;
		return new EventEmitter(v, variant);
	}

}
