package banjo.event.source;

import banjo.event.Event;
import fj.P;
import fj.P2;
import fj.data.List;

public class CompositeEventSource implements EventSource {
	public final List<EventSource> sources;
	
	public CompositeEventSource(List<EventSource> sources) {
		super();
		this.sources = sources;
	}

	@Override
	public P2<EventSource, List<Event>> poll(long timestamp) {
		List<P2<EventSource, List<Event>>> a = sources.map(s -> s.poll(timestamp));
		List<EventSource> newSources = a.map(P2.__1());
		List<Event> newEvents = List.join(a.map(P2.__2()));
		return P.p(this.update(newSources), newEvents);
	}

	public CompositeEventSource update(List<EventSource> newSources) {
		if(newSources == sources || (newSources.length() == sources.length() && newSources.zipWith(sources, (a,b) -> (a == b)).foldLeft((a, b) -> a && b, true)))
			return this;
		return new CompositeEventSource(newSources);
	}

	@Override
	public long nextPollTime(long lastPollTime) {
		return sources.map(s -> s.nextPollTime(lastPollTime)).foldLeft(Math::min, Long.MAX_VALUE);
	}

}
