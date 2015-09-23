package banjo.io.resource;

import banjo.eval.SlotNotFound;
import banjo.event.PastEvent;
import banjo.value.SlotValue;
import banjo.value.Value;
import fj.P;
import fj.P2;
import fj.data.List;

public class CompositeResource extends BaseResource {
	public final List<Resource> resources;
	
	public CompositeResource(List<Resource> resources) {
		this.resources = resources;
	}

	@Override
	public Value slot(Value self, String name, Value fallback) {
		Value v = resources.foldLeft((temp, res) -> (Value)new SlotValue(res, self, name, temp), fallback);
		if(v == null)
			return new SlotNotFound(name, this);
		return v;
	}
	
	@Override
	public void handleEvent(PastEvent event) {
		for(Resource res : resources) {
			res.handleEvent(event);
		}
	}
	
	@Override
	public void watchValue(Value output) {
		for(Resource res : resources) {
			res.watchValue(output);
		}
	}
	
	@Override
	public P2<Resource, List<PastEvent>> poll(long timestamp) {
		List<P2<Resource, List<PastEvent>>> a = resources.map(s -> s.poll(timestamp));
		List<Resource> newSources = a.map(P2.__1());
		List<PastEvent> newEvents = List.join(a.map(P2.__2()));
		CompositeResource newThis = this.update(newSources);
		return P.p(newThis, newEvents);
	}

	public CompositeResource update(List<Resource> newResources) {
		if(newResources == resources || (newResources.length() == resources.length() && newResources.zipWith(resources, (a,b) -> (a == b)).foldLeft((a, b) -> a && b, true)))
			return this;
		return new CompositeResource(newResources);
	}

	@Override
	public long nextPollTime(long lastPollTime) {
		List<Long> componentNextPollTimes = resources.map(s -> s.nextPollTime(lastPollTime));
		Long minNextPollTime = componentNextPollTimes.foldLeft(Math::min, Long.MAX_VALUE);
		return minNextPollTime;
	}

}
