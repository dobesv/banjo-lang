package banjo.io.resource;

import banjo.event.Event;
import banjo.value.Value;
import fj.P;
import fj.P2;
import fj.data.List;

/**
 * Does nothing with the value.  Useful for a starting point of a fold
 * or something like that, but should disappear once the application is running.
 */
public class NullResource extends BaseResource {
	public static final NullResource INSTANCE = new NullResource();
	public static final P2<Resource, List<Event>> POLL_RESULT = P.p(INSTANCE, List.nil());
	
	@Override
	public void accept(Value output) {
	}
	
	@Override
	public long nextPollTime(long lastPollTime) {
		return Long.MAX_VALUE;
	}
	
	@Override
	public P2<Resource, List<Event>> poll(long timestamp) {
		return POLL_RESULT;
	}
	
}
