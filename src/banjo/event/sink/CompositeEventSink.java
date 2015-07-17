package banjo.event.sink;

import banjo.event.Event;
import fj.data.List;

public class CompositeEventSink implements EventSink {
	public final List<EventSink> sinks;
	
	public CompositeEventSink(List<EventSink> sinks) {
		this.sinks = sinks;
	}

	@Override
	public void accept(Event event) {
		for(EventSink sink : sinks) {
			sink.accept(event);
		}
	}

}
