package banjo.eval.environment;

import banjo.value.Value;

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
		return visitor.functionSelfWithBase(function, baseFunction);
	}
	
	public FunctionRecursiveBindingWithBase update(Value function, Value baseFunction) {
		if(function == this.function && baseFunction == this.baseFunction)
			return this;
		return new FunctionRecursiveBindingWithBase(function, baseFunction);
	}

}
