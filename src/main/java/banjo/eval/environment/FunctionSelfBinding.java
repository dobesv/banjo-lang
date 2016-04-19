package banjo.eval.environment;

import banjo.value.Value;

public class FunctionSelfBinding implements Binding {
	public final Value function;
	
	public FunctionSelfBinding(Value function) {
		super();
		this.function = function;
	}

	@Override
	public <T> T acceptVisitor(BindingVisitor<T> visitor) {
        return visitor.functionSelf(function);
	}
	
	public FunctionSelfBinding update(Value function) {
		if(function == this.function)
			return this;
		return new FunctionSelfBinding(function);
	}

}
