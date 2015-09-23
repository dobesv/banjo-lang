package banjo.eval.expr;

import banjo.event.PastEvent;
import banjo.value.Reaction;
import banjo.value.Value;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;

/**
 * A "Free" slot instance doesn't depend on the object, just
 * the environment.  So this just returns the same value every
 * time because we've already bound to the environment.
 */
public class FreeSlotInstance implements SlotInstance {

	public final Value value;
	private ObservableFreeSlotInstance observable;

	public FreeSlotInstance(Value value) {
	    super();
	    this.value = value;
    }

	@Override
	public Value apply(Value t, Value u) {
	    return value;
	}
	
	@Override
	public Reaction<SlotInstance> react(PastEvent event) {
		return value.react(event).map(this::update);
	}

	@Override
	public boolean isReactive() {
		return value.isReactive();
	}
	
	private FreeSlotInstance update(Value newValue) {
		return (value == newValue)? this : new FreeSlotInstance(newValue);
	}

	public SlotInstance withValue(Value newValue) {
		if(newValue == this.value)
			return this;
		return new FreeSlotInstance(newValue);
	}
	
	@Override
	public String toString() {
		return value.toString();
	}

	public static final class ObservableFreeSlotInstance extends ObjectBinding<SlotInstance> {
		final ObservableValue<Value> valueBinding;
		FreeSlotInstance slotInstance;
		public ObservableFreeSlotInstance(FreeSlotInstance slotInstance) {
			super();
			valueBinding = slotInstance.value.toObservableValue();
			bind(valueBinding);
			this.slotInstance = slotInstance;
		}
		
		@Override
		public void dispose() {
			unbind(valueBinding);
		}
		
		@Override
		protected SlotInstance computeValue() {
			return slotInstance = slotInstance.update(valueBinding.getValue());
		}
		
	}
	@Override
	public ObservableValue<SlotInstance> toObservableValue() {
		if(this.observable == null)
			this.observable = new ObservableFreeSlotInstance(this);
		return this.observable;
	}
}
