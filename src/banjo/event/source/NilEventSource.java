package banjo.event.source;

import banjo.event.Event;
import fj.P;
import fj.P2;
import fj.data.List;

public class NilEventSource implements EventSource {
	public static final NilEventSource INSTANCE = new NilEventSource();
	private static final P2<EventSource, List<Event>> pollResult = P.p(INSTANCE, List.nil());
	
	private NilEventSource() {
	}
	
	@Override
	public long nextPollTime(long lastPollTime) {
		return Long.MAX_VALUE;
	}

	@Override
	public P2<EventSource, List<Event>> poll(long timestamp) {
		return pollResult;
	}

}
