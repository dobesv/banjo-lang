package banjo.event.view;

import banjo.event.Event;
import banjo.value.Reaction;
import banjo.value.Value;
import banjo.value.meta.WrapperValue;
import fj.P2;
import fj.data.List;

/**
 * View events as a time-varying value instead.
 * 
 * Called a "behavior" in FRP literature.
 */
public class ValueEventView extends WrapperValue {

	public final EventView source;

	public ValueEventView(EventView source, Value value) {
		super(value);
		this.source = source;
	}

	@Override
	protected Value rewrap(Value newValue) {
		return update(source, newValue);
	}
	
	@Override
	public Reaction<Value> react(Event event) {
		// Not only do we have to update the source and target based on the event, but we also
		// have to pipe the event through the source again to get the new value.
		return Reaction.to(source, target, event).map(p -> update(p._1(),p._2(),event));
	}
	
	/**
	 * Update event source state and value based on a incoming event.
	 */
	public Value update(EventView source, Value value, Event event) {
		P2<EventView, List<Value>> p = source.apply(event);			
		EventView newSource = p._1();
		Value newValue = p._2().isEmpty() ? value : p._2().last();
		return update(newSource, newValue);
	}
	
	public Value update(EventView newSource, Value newValue) {
		if(newSource == this.source && newValue == this.target)
			return this;
		return new ValueEventView(newSource, newValue);
	}
	
}