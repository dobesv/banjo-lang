package banjo.event;

import java.util.function.Consumer;
import java.util.function.Function;

import com.sun.javafx.binding.ExpressionHelper;

import banjo.expr.source.Operator;
import banjo.io.resource.BaseResource;
import banjo.io.resource.Resource;
import banjo.value.FunctionTrait;
import banjo.value.Reaction;
import banjo.value.Value;
import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;

public class FutureEvent extends FunctionTrait implements Value, Function<Value, Value> {
	final String variant;
	private ObservableFutureEvent observable;
	
	public FutureEvent(String variant) {
		this.variant = variant;
	}

	@Override
	public Reaction<Value> react(PastEvent event) {
		if(variant.equals(event.variant)) {
			return Reaction.of(event);
		}
		return Reaction.of(this);
	}

	@Override
	public boolean isReactive() {
		return true;
	}

	@Override
	public Value apply(Value t) {
		return t.slot("pending");
	}

	@Override
	public Value slot(Value self, String name, Value fallback) {
		switch(Operator.fromMethodName(name, true)) {
		case FALLBACK:
			return Value.function(Function.identity());
		case FUNCTION_COMPOSITION_RIGHT:
		case FUNCTION_COMPOSITION_LEFT: 
			return Value.function(x -> this);
		default:		
			if("timestamp".equals(name)) {
				return Value.fromJava(Long.MAX_VALUE); // TODO Could/should be ∞  or -∞ instead ?
			}
			return super.slot(self, name, fallback);
		}
	}
	
	public static final class ObservableFutureEvent implements ObservableValue<Value>, Consumer<PastEvent> {
		private ExpressionHelper<Value> helper = null;
		Value pastEvent;
		final FutureEvent futureEvent;

		public ObservableFutureEvent(FutureEvent futureEvent) {
			this.futureEvent = futureEvent;
		}

	    @Override 
	    public void addListener(InvalidationListener listener) {
	    	if(pastEvent != null)
	    		pastEvent.toObservableValue().addListener(listener);
	    	else
	    		helper = ExpressionHelper.addListener(helper, this, listener);
	    }

	    @Override 
	    public void removeListener(InvalidationListener listener) {
	    	if(pastEvent != null)
	    		pastEvent.toObservableValue().removeListener(listener);
	    	else
	    		helper = ExpressionHelper.removeListener(helper, listener);
	    }
	    
	    @Override
	    public void addListener(ChangeListener<? super Value> listener) {
	    	if(pastEvent != null)
	    		pastEvent.toObservableValue().addListener(listener);
	    	else
	    		helper = ExpressionHelper.addListener(helper, this, listener);
	    }

	    @Override 
	    public void removeListener(ChangeListener<? super Value> listener) {
	    	if(pastEvent != null)
	    		pastEvent.toObservableValue().removeListener(listener);
	    	else
	    		helper = ExpressionHelper.removeListener(helper, listener);
	    }
	    
		@Override
		public Value getValue() {
			return pastEvent == null ? futureEvent : pastEvent;
		}
		
		@Override
		public void accept(PastEvent t) {
			pastEvent = t;
			ExpressionHelper.fireValueChangedEvent(helper);
			if(helper != null) {
				ObservableValue<Value> po = pastEvent.toObservableValue();
				po.addListener(new ChangeListener<Value>() {
					public void changed(javafx.beans.value.ObservableValue<? extends Value> observable, Value oldValue, Value newValue) {
						pastEvent = newValue;
						ExpressionHelper.fireValueChangedEvent(helper);
					};
				});
				po.addListener(new InvalidationListener() {
					@Override
					public void invalidated(Observable observable) {
						ExpressionHelper.fireValueChangedEvent(helper);
					}
				});
			}
			futureEvent.observable = null; // Detach from futureEvent, now we're attached to pastEvent
		}
	}
	
	@Override
	public ObservableFutureEvent toObservableValue() {
		if(observable == null)
			observable = new ObservableFutureEvent(this);
		return observable;
	}

	public PastEvent occurrence(long timestamp, Value arg) {
		return new PastEvent(timestamp, variant, arg);
	}

	/**
	 * Resource that pushes updates to this event to listeners, in response to events.
	 */
	public static final class FutureEventResource extends BaseResource implements Resource {
		final FutureEvent futureEvent;
		
		public FutureEventResource(FutureEvent futureEvent) {
			super();
			this.futureEvent = futureEvent;
		}

		@Override
		public void handleEvent(PastEvent event) {
			if(futureEvent.observable != null && event.variant.equals(futureEvent.variant))
				futureEvent.observable.accept(event);
		}

		@Override
		public void watchValue(Value output) {
			
		}

	}
	public Resource toResource() {
		return new FutureEventResource(this);
	}
}
