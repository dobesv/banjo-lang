package banjo.event.view;

import java.util.function.Function;

import banjo.event.Event;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.P2;
import fj.data.List;

public class TransformingEventView implements EventView {
	final EventView source;
	final Value function;

	public TransformingEventView(EventView source, Value function) {
		super();
		this.source = source;
		this.function = function;
	}

	@Override
	public P2<EventView, List<Value>> apply(Event t) {
		return source.apply(t).split(
				newSource -> this.update(newSource, function), 
				evts -> evts.map(function::call1));
	}
	
	@Override
	public Reaction<EventView> react(Event event) {
		return Reaction.to(source, function, event).map(P2.tuple(this::update));
	}
	
	public EventView update(EventView newSource, Value newFunction) {
		if(source == newSource && function == newFunction)
			return this;
		return new TransformingEventView(newSource, newFunction);
	}
}