package banjo.value;

import banjo.event.Event;

/**
 * Base class for Value classes that don't "step" state in response to events.
 */
public class BaseInertValue implements Value {

	@Override
	public Reaction<Value> react(Event event) {
		return Reaction.none(this);
	}
}
