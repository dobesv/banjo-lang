package banjo.eval.environment;

import banjo.value.Value;

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
	
	public FunctionRecursiveBinding update(Value function) {
		if(function == this.function)
			return this;
		return new FunctionRecursiveBinding(function);
	}

}
