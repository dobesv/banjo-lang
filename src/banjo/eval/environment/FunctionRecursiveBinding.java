package banjo.eval.environment;

import com.sun.javafx.binding.ObjectConstant;

import banjo.event.Event;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.P2;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;

public class FunctionRecursiveBinding implements Binding {
	public final Value function;
	
	public FunctionRecursiveBinding(Value function) {
		super();
		this.function = function;
	}

	@Override
	public <T> T acceptVisitor(BindingVisitor<T> visitor) {
		return visitor.functionRecursive(function);
	}
	
	@Override
	public Reaction<Binding> react(Event event) {
		return Reaction.to(function, event).map(this::update);
	}

	@Override
	public boolean isReactive() {
		return function.isReactive();
	}
	
	public FunctionRecursiveBinding update(Value function) {
		if(function == this.function)
			return this;
		return new FunctionRecursiveBinding(function);
	}

	public static final class ObservableBoundFunctionSelf extends ObjectBinding<Binding> {
		public final ObservableValue<Value> functionBinding;
		FunctionRecursiveBinding boundFunctionSelf;
		
		public ObservableBoundFunctionSelf(FunctionRecursiveBinding boundFunctionSelf) {
			functionBinding = boundFunctionSelf.function.toObservableValue();
			bind(functionBinding);
			this.boundFunctionSelf = boundFunctionSelf;
		}
		
		@Override
		public void dispose() {
			unbind(functionBinding);
		}
		
		@Override
		protected Binding computeValue() {
			return boundFunctionSelf = boundFunctionSelf.update(functionBinding.getValue());
		}
		
	}
	@Override
	public ObservableValue<Binding> toObservableValue() {
		if(!isReactive())
			return ObjectConstant.valueOf(this);
		return new ObservableBoundFunctionSelf(this);
	}

}
