package banjo.event.source;

import java.util.ServiceLoader;

import banjo.eval.util.SlotName;
import banjo.event.Event;
import fj.P2;
import fj.data.List;

public interface EventSource {
	public long nextPollTime(long lastPollTime);

	public P2<EventSource, List<Event>> poll(long timestamp);
	
	public static EventSource discoveredEventSourcesSource() {
		ServiceLoader<EventSource> found = ServiceLoader.load(EventSource.class);
		List<EventSource> sources = List.iterableList(found);
		return new CompositeEventSource(sources);
	}
	
	public static EventSource of(Event event) {
		return new OneOffEventSource(event);
	}
	
	static List<EventSource> flatten(EventSource e) {
		if(e == NilEventSource.INSTANCE)
			return List.nil();
		if(e instanceof CompositeEventSource)
			return ((CompositeEventSource)e).sources;
		return List.single(e);
	}
	public static EventSource merge(List<EventSource> sources) {
		// Flatten out the list of sources
		sources = List.join(sources.map(EventSource::flatten));
		if(sources.isEmpty())
			return NilEventSource.INSTANCE;
		if(sources.isSingle())
			return sources.head();
		return new CompositeEventSource(sources);
	}
	
	@SlotName("∪")
	public default EventSource union(EventSource other) {
		return merge(List.list(this, other));
	}
}
