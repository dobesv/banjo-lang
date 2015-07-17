package banjo.event.view;

import banjo.event.Event;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.P;
import fj.P2;
import fj.data.List;

public class CompositeEventView implements EventView {
	public final List<EventView> sources;
	
	public CompositeEventView(List<EventView> sources) {
		super();
		this.sources = sources;
	}

	@Override
	public P2<EventView, List<Value>> apply(Event t) {
		List<P2<EventView, List<Value>>> a = sources.map(s -> s.apply(t));
		List<EventView> newSources = a.map(P2.__1());
		List<Value> values = List.join(a.map(P2.__2()));
		return P.p(this.update(newSources), values);
	}

	@Override
	public Reaction<EventView> react(Event event) {
		return Reaction.to(sources, event).map(this::update);
	}
	
	public EventView update(List<EventView> sources) {
		if(sources == this.sources)
			return this;
		return new CompositeEventView(sources);
	}

}
