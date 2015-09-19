package banjo.value;

import com.sun.javafx.binding.ObjectConstant;

import banjo.event.Event;
import javafx.beans.value.ObservableValue;

/**
 * Base class for Value classes that don't "step" state in response to events.
 */
public class BaseInertValue implements Value {

	@Override
	public Reaction<Value> react(Event event) {
		return Reaction.of(this);
	}

	@Override
	public boolean isReactive() {
		return false;
	}
	
	@Override
	public ObservableValue<Value> toObservableValue() {
		return ObjectConstant.valueOf(this);
	}
}
