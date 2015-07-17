package banjo.value;

import banjo.event.Event;

public interface Reactive<T> {
	/**
	 * Step the value in response to an event occurring.  Return the same
	 * object if the event doesn't affect this value.  The default implementation
	 * return the same object again.
	 */
	public Reaction<T> react(Event event);
}
