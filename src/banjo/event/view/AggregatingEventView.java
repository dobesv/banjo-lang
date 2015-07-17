package banjo.event.view;

import java.util.function.BiFunction;

import banjo.event.Event;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.P;
import fj.P2;
import fj.data.List;

public class AggregatingEventView implements EventView {
	final EventView source;
	final BiFunction<Value,Value,Value> f;
	final Value state;

	public AggregatingEventView(EventView source, BiFunction<Value,Value,Value> f, Value state) {
		this.source = source;
		this.f = f;
		this.state = state;
	}

	
	/**
	 * Maintain an internal state - when new events are received we apply
	 * a function to the previous event value and the new one for each event
	 * we receive.  An initial value is provided to use as the "previous" value
	 * on the first call.
	 * 
	 * This outputs events at the same time as the source does.
	 */
	@Override
	public P2<EventView, List<Value>> apply(Event t) {
		P2<EventView, List<Value>> p = source.apply(t);
		EventView newSource = p._1();
		List<Value> newEvents = p._2();
		// Have to produce the same number of values, in case they are counting
		List<Value> newStates = List.nil();
		Value newState = state;
		for(Value ev : newEvents) {
			newState = f.apply(newState, ev);
			newStates.cons(newState);
		}
		return P.p(this.update(newSource, newStates.isEmpty() ? state : newStates.last()), newStates.reverse());
	}
	
	@Override
	public Reaction<EventView> react(Event event) {
		return Reaction.to(source, state, event).map(P2.tuple(this::update));
	}
	
	public EventView update(EventView newSource, Value newState) {
		if(source == newSource && state == newState)
			return this;
		return new AggregatingEventView(newSource, f, newState);
	}
}