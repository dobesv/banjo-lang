package banjo.eval.environment;

import banjo.event.Event;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.P2;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;

public class SlotSelfBindingWithBase implements Binding {

	public final Value sourceObject;
	public final String slotName;
	public final Value baseSlotValue;

	public SlotSelfBindingWithBase(Value sourceObject, String slotName, Value baseSlotValue) {
		this.sourceObject = sourceObject;
		this.slotName = slotName;
		this.baseSlotValue = baseSlotValue;
	}

	@Override
	public Reaction<Binding> react(Event event) {
		return Reaction.to(sourceObject, baseSlotValue, event).map(P2.tuple(this::update));
	}

	@Override
	public boolean isReactive() {
		return sourceObject.isReactive() || baseSlotValue.isReactive();
	}

	public SlotSelfBindingWithBase update(Value sourceObject, Value baseSlotValue) {
		if(sourceObject == this.sourceObject && baseSlotValue == this.baseSlotValue)
			return this;
		return new SlotSelfBindingWithBase(sourceObject, slotName, baseSlotValue);
	}
	
	public static final class ObservableSlotSelfBinding extends ObjectBinding<Binding> {
		final ObservableValue<Value> sourceObjectBinding;
		final ObservableValue<Value> baseSlotValueBinding;
		SlotSelfBindingWithBase slotSelfBinding;
		
		public ObservableSlotSelfBinding(SlotSelfBindingWithBase slotSelfBinding) {
			super();
			sourceObjectBinding = slotSelfBinding.sourceObject.toObservableValue();
			baseSlotValueBinding = slotSelfBinding.baseSlotValue.toObservableValue();
			bind(sourceObjectBinding, baseSlotValueBinding);
			this.slotSelfBinding = slotSelfBinding;
		}
		
		@Override
		public void dispose() {
			unbind(sourceObjectBinding, baseSlotValueBinding);
		}
		
		@Override
		protected Binding computeValue() {
			return slotSelfBinding = slotSelfBinding.update(sourceObjectBinding.getValue(), baseSlotValueBinding.getValue());
		}
		
	}
	
	
	@Override
	public ObservableValue<Binding> toObservableValue() {
		return new ObservableSlotSelfBinding(this);
	}

	@Override
	public <T> T acceptVisitor(BindingVisitor<T> visitor) {
		return visitor.slotWithBase(sourceObject, slotName, baseSlotValue);
	}

}
