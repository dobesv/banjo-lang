package banjo.event.view;

import banjo.event.Event;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.P;
import fj.P2;
import fj.data.List;

public class CountingEventView implements EventView {
	final EventView source;
	final long count;

	public CountingEventView(EventView source, long count) {
		this.source = source;
		this.count = count;
	}

	@Override
	public P2<EventView, List<Value>> apply(Event t) {
		P2<EventView, List<Value>> a = source.apply(t);
		EventView newSource = a._1();
		long newCount = count;
		List<Value> outputs = List.nil();
		for(Value v : a._2()) {
			newCount += 1;
			outputs = outputs.cons(Value.fromJava(newCount));
		}
		return P.p(this.update(newSource, newCount), outputs.reverse());
	}

	@Override
	public Reaction<EventView> react(Event event) {
		return source.react(event).map(source -> this.update(source, count));
	}

	public EventView update(EventView source, long count) {
		if(source == this.source && count == this.count)
			return this;
		return new CountingEventView(source, count);
	}
}
