package banjo.event.view;

import java.util.function.Predicate;

import banjo.event.Event;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.P2;
import fj.data.List;

public class FilteringEventView implements EventView {
	final EventView source;
	final Value predicate;

	public FilteringEventView(EventView source, Value predicate) {
		super();
		this.source = source;
		this.predicate = predicate;
	}

	
	@Override
	public P2<EventView, List<Value>> apply(Event t) {
		return source.apply(t).split(
				newSource -> this.update(newSource, predicate), 
				evts -> evts.filter(e -> predicate.call1(e).isTruthy()));
	}
	
	@Override
	public Reaction<EventView> react(Event event) {
		return Reaction.to(source, predicate, event).map(P2.tuple(this::update));
	}
	
	public EventView update(EventView newSource, Value newPredicate) {
		if(source == newSource && predicate == newPredicate)
			return this;
		return new FilteringEventView(newSource, newPredicate);
	}
}