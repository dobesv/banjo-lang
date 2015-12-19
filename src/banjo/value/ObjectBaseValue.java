package banjo.value;

import banjo.event.PastEvent;
import banjo.expr.util.SourceFileRange;
import fj.data.Set;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;

/**
 * An object with a single slot - which is the base slot value
 * for a field.  Used to implement "base" or "super" slot
 * projections.
 */
public class ObjectBaseValue implements Value {
	public final String selfName;
	public final String slotName;
	public final Value slotValue;
	private ObservableObjectBaseValue observable;
	
	public ObjectBaseValue(String selfName, String slotName, Value slotValue) {
		this.selfName = selfName;
		this.slotName = slotName;
		this.slotValue = slotValue;
	}
	
	@Override
	public Value slot(String name, Set<SourceFileRange> ranges) {
		if(name.equals(slotName))
			return slotValue;
		return Value.super.slot(name, ranges);
	}
	
	@Override
	public Value slot(Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
		if(name.equals(slotName))
			return slotValue;
		return Value.super.slot(self, name, ranges, fallback);
	}

	@Override
	public boolean isReactive() {
		return slotValue.isReactive();
	}
	
	@Override
	public Reaction<Value> react(PastEvent event) {
		return slotValue.react(event).map(this::update);
	}
	
	public ObjectBaseValue update(Value newSlotValue) {
		if(newSlotValue == slotValue)
			return this;
		return new ObjectBaseValue(selfName, slotName, newSlotValue);
	}
	
	@Override
	public String toString() {
		return selfName+":"+slotName;
	}

	public static final class ObservableObjectBaseValue extends ObjectBinding<Value> {
		public final ObservableValue<Value> slotValueBinding;
		ObjectBaseValue objectBaseValue;
		
		public ObservableObjectBaseValue(ObjectBaseValue objectBaseValue) {
			slotValueBinding = objectBaseValue.toObservableValue();
			this.objectBaseValue = objectBaseValue;
			
			bind(slotValueBinding);
		}
		
		@Override
		protected Value computeValue() {
			return objectBaseValue = objectBaseValue.update(slotValueBinding.getValue());
		}
		
	}
	
	@Override
	public ObservableValue<Value> toObservableValue() {
		if(observable == null)
			observable = new ObservableObjectBaseValue(this);
		return observable;
	}
}
