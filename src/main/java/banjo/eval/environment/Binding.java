package banjo.eval.environment;

import java.util.Objects;

import banjo.value.Reactive;
import banjo.value.Value;
import javafx.beans.value.ObservableValue;

public interface Binding extends Reactive<Binding> {

	ObservableValue<Binding> toObservableValue();
	
	public static Binding let(Value value) {
		Objects.requireNonNull(value);
		return new LetBinding(value);
	}
	
	public static Binding functionSelf(Value function) {
		Objects.requireNonNull(function);
		return new FunctionRecursiveBinding(function);
	}
	public static Binding functionSelfWithBase(Value function, Value baseFunction) {
		Objects.requireNonNull(function);
		Objects.requireNonNull(baseFunction);
		return new FunctionRecursiveBindingWithBase(function, baseFunction);
	}	
	public static Binding slot(Value sourceObject, String slotName) {
		Objects.requireNonNull(sourceObject);
		Objects.requireNonNull(slotName);
		return new SlotSelfBinding(sourceObject, slotName);
		
	}
	
	public static Binding slotWithBase(Value sourceObject, String slotName, Value baseSlotValue) {
		Objects.requireNonNull(sourceObject);
		Objects.requireNonNull(slotName);
		Objects.requireNonNull(baseSlotValue);		
		return new SlotSelfBindingWithBase(sourceObject, slotName, baseSlotValue);
	}
	
	public <T> T acceptVisitor(BindingVisitor<T> visitor);
	
	public default Value getValue() {
		return acceptVisitor(new BindingVisitor<Value>() {
			@Override
			public Value let(Value value) {
				return value;
			}
			
			@Override
			public Value functionRecursive(Value function) {
				return function;
			}
			
			@Override
			public Value functionRecursiveWithBase(Value function, Value baseFunction) {
				return function;
			}
			
			@Override
			public Value slot(Value sourceObject, String slotName) {
				return sourceObject;
			}
			
			@Override
			public Value slotWithBase(Value sourceObject, String slotName, Value baseSlotValue) {
				return sourceObject;
			}
		});
	}
}
