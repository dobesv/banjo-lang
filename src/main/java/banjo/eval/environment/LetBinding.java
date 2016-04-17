package banjo.eval.environment;

import banjo.value.Value;

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

	public LetBinding update(Value newValue) {
		if(value == newValue)
			return this;
		return new LetBinding(newValue);
	}
}
