package banjo.event.view;

import banjo.event.Event;
import banjo.event.source.EventSource;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.P;
import fj.P2;
import fj.data.List;

/**
 * Emit events based on a translation of events from an EventView.
 */
public class EventFactoryEventView implements EventView {

	public final EventView source;
	public final String variant;
	
	
	public EventFactoryEventView(EventView source, String variant) {
		super();
		this.source = source;
		this.variant = variant;
	}

	@Override
	public P2<EventView, List<Value>> apply(Event t) {
		return source.apply(t).split(this::update, values -> values.map(v -> new Event(t.timestamp, variant, v)));
	}
	
	@Override
	public Reaction<EventView> react(Event event) {
		return source.react(event).map(this::update);
	}
	
	public EventView update(EventView newSource) {
		if(source == newSource)
			return this;
		return new EventFactoryEventView(newSource, variant);
	}

}
