package banjo.eval.value;

import banjo.eval.interceptors.CallResultInterceptor;

/**
 * Supply slots for function composition
 */
public class FunctionTrait extends ValueToStringTrait implements Value {


	@Override
	public Value slot(Value self, String name, Value fallback) {
		if("；".equals(name)) {
			return Value.function(this::composeRight);
		} else if("∘".equals(name)) {
			return Value.function(this::composeLeft);
		} else {
			return Value.super.slot(self, name, fallback);
		}
	}

	public Value composeRight(Value functionAfter) {
		return new CallResultInterceptor(functionAfter, this);
	}

	public Value composeLeft(Value functionBefore) {
		return new CallResultInterceptor(this, functionBefore);
	}
}
