package banjo.eval.environment;

import com.sun.javafx.binding.ObjectConstant;

import banjo.event.PastEvent;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.P2;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;

public class FunctionRecursiveBindingWithBase implements Binding {
	public final Value function;
	public final Value baseFunction; // May be null if this doesn't override anything
	
	public FunctionRecursiveBindingWithBase(Value function, Value baseFunction) {
		super();
		this.function = function;
		this.baseFunction = baseFunction;
	}

	@Override
	public <T> T acceptVisitor(BindingVisitor<T> visitor) {
		return visitor.functionRecursiveWithBase(function, baseFunction);
	}
	
	@Override
	public Reaction<Binding> react(PastEvent event) {
		return Reaction.to(function, baseFunction, event).map(P2.tuple(this::update));
	}

	@Override
	public boolean isReactive() {
		return function.isReactive() || (baseFunction != null && baseFunction.isReactive());
	}
	
	public FunctionRecursiveBindingWithBase update(Value function, Value baseFunction) {
		if(function == this.function && baseFunction == this.baseFunction)
			return this;
		return new FunctionRecursiveBindingWithBase(function, baseFunction);
	}

	public static final class ObservableBoundFunctionSelf extends ObjectBinding<Binding> {
		public final ObservableValue<Value> functionBinding;
		public final ObservableValue<Value> baseFunctionBinding;
		FunctionRecursiveBindingWithBase boundFunctionSelf;
		
		public ObservableBoundFunctionSelf(FunctionRecursiveBindingWithBase boundFunctionSelf) {
			functionBinding = boundFunctionSelf.function.toObservableValue();
			baseFunctionBinding = boundFunctionSelf.baseFunction.toObservableValue();
			bind(functionBinding, baseFunctionBinding);
			this.boundFunctionSelf = boundFunctionSelf;
		}
		
		@Override
		public void dispose() {
			unbind(functionBinding, baseFunctionBinding);
		}
		
		@Override
		protected Binding computeValue() {
			return boundFunctionSelf = boundFunctionSelf.update(functionBinding.getValue(), baseFunctionBinding.getValue());
		}
		
	}
	@Override
	public ObservableValue<Binding> toObservableValue() {
		if(!isReactive())
			return ObjectConstant.valueOf(this);
		return new ObservableBoundFunctionSelf(this);
	}

}
