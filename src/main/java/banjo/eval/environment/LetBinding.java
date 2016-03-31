package banjo.eval.environment;

import banjo.event.PastEvent;
import banjo.value.Reaction;
import banjo.value.Value;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;

public class LetBinding implements Binding {
	public final Value value;
	
	public LetBinding(Value value) {
		super();
		this.value = value;
	}
	
	@Override
	public <T> T acceptVisitor(BindingVisitor<T> visitor) {
		return visitor.let(value);
	}

	@Override
	public Reaction<Binding> react(PastEvent event) {
		return value.react(event).map(this::update);
	}

	@Override
	public boolean isReactive() {
		return value.isReactive();
	}

	public LetBinding update(Value newValue) {
		if(value == newValue)
			return this;
		return new LetBinding(newValue);
	}
	
	public static final class ObservableLetBoundValue extends ObjectBinding<Binding> {
		public final ObservableValue<Value> valueBinding;
		LetBinding letBoundValue;
		public ObservableLetBoundValue(LetBinding letBoundValue) {
			super();
			valueBinding = letBoundValue.value.toObservableValue();
			bind(valueBinding);
			this.letBoundValue = letBoundValue;
		}
		
		@Override
		public void dispose() {
			unbind(valueBinding);
		}
		
		@Override
		protected Binding computeValue() {
			return letBoundValue = letBoundValue.update(valueBinding.getValue());
		}
		
	}
	
	@Override
	public ObservableValue<Binding> toObservableValue() {
		return new ObservableLetBoundValue(this);
	}

}
