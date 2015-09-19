package banjo.eval.environment;

import banjo.event.Event;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.P2;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;

public class SlotSelfBinding implements Binding {

	public final Value sourceObject;
	public final String slotName;

	public SlotSelfBinding(Value sourceObject, String slotName) {
		this.sourceObject = sourceObject;
		this.slotName = slotName;
	}

	@Override
	public Reaction<Binding> react(Event event) {
		return Reaction.to(sourceObject, event).map(this::update);
	}

	@Override
	public boolean isReactive() {
		return sourceObject.isReactive();
	}

	public SlotSelfBinding update(Value sourceObject) {
		if(sourceObject == this.sourceObject)
			return this;
		return new SlotSelfBinding(sourceObject, slotName);
	}
	
	public static final class ObservableSlotSelfBinding extends ObjectBinding<Binding> {
		final ObservableValue<Value> sourceObjectBinding;
		SlotSelfBinding slotSelfBinding;
		
		public ObservableSlotSelfBinding(SlotSelfBinding slotSelfBinding) {
			super();
			sourceObjectBinding = slotSelfBinding.sourceObject.toObservableValue();
			bind(sourceObjectBinding);
			this.slotSelfBinding = slotSelfBinding;
		}
		
		@Override
		public void dispose() {
			unbind(sourceObjectBinding);
		}
		
		@Override
		protected Binding computeValue() {
			return slotSelfBinding = slotSelfBinding.update(sourceObjectBinding.getValue());
		}
		
	}
	
	
	@Override
	public ObservableValue<Binding> toObservableValue() {
		return new ObservableSlotSelfBinding(this);
	}

	@Override
	public <T> T acceptVisitor(BindingVisitor<T> visitor) {
		return visitor.slot(sourceObject, slotName);
	}

}
