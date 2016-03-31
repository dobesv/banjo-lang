package banjo.value;

import com.sun.javafx.binding.ObjectConstant;

import banjo.event.PastEvent;
import javafx.beans.value.ObservableValue;

public interface Reactive<T extends Reactive<T>> {
	/**
	 * Step the value in response to an event occurring.  Return the same
	 * object if the event doesn't affect this value.  The default implementation
	 * return the same object again.
	 */
    @SuppressWarnings("unchecked")
    public default Reaction<T> react(PastEvent event) {
        return Reaction.<T> of((T) this);
	}
	
	/**
	 * If isReactive() returns false, that means that calling react() will always be a no-op (returns this) for this value.
	 */
    public default boolean isReactive() {
        return false;
    }
	
	/**
	 * @return a javafx observable value for push-based updates
	 */
    @SuppressWarnings("unchecked")
    public default ObservableValue<T> toObservableValue() {
        return ObjectConstant.<T> valueOf((T) this);
    }
}
