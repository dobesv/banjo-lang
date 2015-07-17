package banjo.event.sink;

import java.util.ServiceLoader;
import java.util.function.Consumer;

import banjo.event.Event;
import fj.data.List;

public interface EventSink extends Consumer<Event> {
	public static EventSink discoveredEventSinksSink() {
		List<EventSink> sinks = List.iterableList(ServiceLoader.load(EventSink.class));
		return new CompositeEventSink(sinks);
	}
	
	public static List<EventSink> flatten(EventSink sink) {
		if(sink == NullEventSink.INSTANCE) {
			return List.nil();
		} else if(sink instanceof CompositeEventSink) {
			return ((CompositeEventSink)sink).sinks;
		} else {
			return List.single(sink);
		}
	}
	public static EventSink merge(List<EventSink> sinks) {
		sinks = List.join(sinks.map(EventSink::flatten));
		if(sinks.isEmpty())
			return NullEventSink.INSTANCE;
		if(sinks.isSingle())
			return sinks.head();
		return new CompositeEventSink(sinks);
	}
}
