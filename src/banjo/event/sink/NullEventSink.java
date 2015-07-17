package banjo.event.sink;

import banjo.event.Event;

public class NullEventSink implements EventSink {
	public static final NullEventSink INSTANCE = new NullEventSink();
	@Override
	public void accept(Event t) {
	}
}
