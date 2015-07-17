package banjo.value;

import banjo.value.meta.FunctionComposition;

/**
 * Supply slots for function composition
 */
public abstract class FunctionTrait extends ValueToStringTrait implements Value {


	@Override
	public Value slot(Value self, String name, Value fallback) {
		if("；".equals(name) || "∘".equals(name)) {
			return Value.function(this::compose);
		} else {
			return Value.super.slot(self, name, fallback);
		}
	}

	public Value compose(Value functionAfter) {
		return new FunctionComposition(functionAfter, this);
	}
}
