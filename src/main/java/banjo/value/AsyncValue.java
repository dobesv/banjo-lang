package banjo.value;

import banjo.event.PastEvent;
import banjo.value.meta.WrapperValue;
import fj.data.List;

/**
 * An event value is a behavior that waits for a particular event to occur.  Once the
 * event has occurred, this becomes that value, in a way.
 */
public class AsyncValue extends WrapperValue {
	// A function that returns whether a particular event is the one we've been waiting for
	final Value eventPredicate;
	
	// When the event occurs, this function is used to wrap the event in something the application wants
	// to deal with (like a singleton list or whatever)
	final Value valueConstructor;
	
	/**
	 * Construct a future value.
	 * 
	 * @param placeholder Value to use until the event occurs
	 * @param eventPredicate A function that returns whether an event passed to it is the one we're waiting for
	 * @param valueConstructor Something that will wrap or transform a raw Java event to something useful
	 */
	public AsyncValue(Value placeholder, Value eventPredicate, Value valueConstructor) {
		super(placeholder);
		this.eventPredicate = eventPredicate;
		this.valueConstructor = valueConstructor;
	}

	@Override
	public Reaction<Value> react(PastEvent event) {
		return Reaction.to(target, eventPredicate, valueConstructor, event).map(p -> this.update(p._1(), p._2(), p._3(), event));
	}
	
	public Value update(Value newTarget, Value newPredicate, Value newCtor, PastEvent event) {
		if(newPredicate.call(List.single(event)).isTruthy()) {
			return newCtor.call(List.single(event));
		}
		return update(newTarget, newPredicate, newCtor);
	}

	public AsyncValue update(Value newTarget, Value newPredicate, Value newCtor) {
		if(newPredicate == this.eventPredicate && newTarget == this.target && newCtor == this.valueConstructor)
			return this;
		return new AsyncValue(newTarget, newPredicate, newCtor);
	}

	@Override
	protected AsyncValue rewrap(Value newValue) {
		return update(newValue, this.eventPredicate, this.valueConstructor);
	}
	
	@Override
	public boolean isReactive() {
		return true;
	}
}
