package banjo.value;

import java.util.function.Consumer;

import com.sun.javafx.binding.ExpressionHelper;

import banjo.event.PastEvent;
import javafx.beans.InvalidationListener;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;

/**
 * Create an ObservableValue<T> for a Reactive<T>.  The reactive value will be updated in response
 * to events, which are provided by calling accept() on the instance of ObservableReactive.
 */
public final class ObservableReactive<T extends Reactive<T>> implements ObservableValue<T>, Consumer<PastEvent> {
    private ExpressionHelper<T> helper = null;
	
	T value;
	
	public ObservableReactive(T value) {
		this.value = value;
	}

    @Override 
    public void addListener(InvalidationListener listener) {
        helper = ExpressionHelper.addListener(helper, this, listener);
    }

    @Override 
    public void removeListener(InvalidationListener listener) {
        helper = ExpressionHelper.removeListener(helper, listener);
    }
    
    @Override
    public void addListener(ChangeListener<? super T> listener) {
        helper = ExpressionHelper.addListener(helper, this, listener);
    }

    @Override 
    public void removeListener(ChangeListener<? super T> listener) {
        helper = ExpressionHelper.removeListener(helper, listener);
    }
    
	@Override
	public T getValue() {
		return value;
	}
	
	@Override
	public void accept(PastEvent t) {
		value = value.react(t).v;
		ExpressionHelper.fireValueChangedEvent(helper);
		
		// If the value becomes inert, discard all listeners
		if(!value.isReactive()) {
			helper = null;
		}
	}
}