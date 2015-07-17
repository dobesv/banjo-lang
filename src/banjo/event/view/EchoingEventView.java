package banjo.event.view;

import banjo.event.Event;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.P;
import fj.P2;
import fj.P3;
import fj.data.List;

public class EchoingEventView implements EventView {

	public EventView source;
	public long period;
	public int count;
	public List<P3<Long,Integer,Value>> queue;

	public EchoingEventView(EventView source, long period, int count, List<P3<Long,Integer,Value>> queue) {
		this.source = source;
		this.period = period;
		this.count = count;
		this.queue = queue;
	}
	
	public EchoingEventView(EventView source, long period, int count) {
		this(source, period, count, List.nil());
	}
	
	public EchoingEventView(EventView source, long period) {
		this(source, period, -1, List.nil());
	}

	@Override
	public P2<EventView, List<Value>> apply(Event event) {
		P2<EventView, List<Value>> p = source.apply(event);
		List<Value> newEvents = p._2();
		P2<List<P3<Long, Integer, Value>>, List<P3<Long, Integer, Value>>> splitQueue = queue.partition(qp -> qp._1() <= event.timestamp);
		List<P3<Long, Integer, Value>> repeatingEvents = splitQueue._1()
				.filter(pe -> pe._2() > 1)
				.map(pe -> pe.map1(ts -> ts + period).map2(c -> c > 0 ? c - 1 : c));
		List<P3<Long, Integer, Value>> futureEvents = splitQueue._2().append(repeatingEvents);
		List<P3<Long,Integer,Value>> newQueue = futureEvents.append(newEvents.map(v -> P.p(event.timestamp + period, count-1, v)));
		List<Value> readyEvents = splitQueue._1().map(P3.__3());
		EventView newSource = p._1();
		return P.p(update(newSource, newQueue), newEvents.append(readyEvents));
	}

	public long minExpiry() {
		return queue.map(P3.__1()).foldRight(Math::min, Long.MAX_VALUE);
	}
	@Override
	public Reaction<EventView> react(Event event) {
		return source.react(event).map(this::update).maybeExpiring(this.minExpiry());
	}
	
	public EventView update(EventView newSource) {
		if(newSource == this.source)
			return this;
		return new EchoingEventView(newSource, period, count, queue);
	}

	public EventView update(EventView newSource, List<P3<Long,Integer,Value>> newQueue) {
		if(newSource == this.source && newQueue == this.queue)
			return this;
		return new EchoingEventView(newSource, period, count, newQueue);
	}
	
}
