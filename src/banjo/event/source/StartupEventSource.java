package banjo.event.source;

import banjo.event.Event;
import fj.P;
import fj.P2;
import fj.data.List;

public class StartupEventSource implements EventSource {

	public static final String STARTUP_EVENT_VARIANT = "startup";

	@Override
	public long nextPollTime(long lastPollTime) {
		return lastPollTime;
	}

	@Override
	public P2<EventSource, List<Event>> poll(long timestamp) {
		return P.p(NilEventSource.INSTANCE, List.single(new Event(timestamp, STARTUP_EVENT_VARIANT)));
	}
}
