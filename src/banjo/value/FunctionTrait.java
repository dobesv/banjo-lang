package banjo.value;

import banjo.expr.util.SourceFileRange;
import banjo.value.meta.FunctionComposition;
import fj.data.Set;

/**
 * Supply slots for function composition
 */
public abstract class FunctionTrait extends ValueToStringTrait implements Value {


	@Override
	public Value slot(Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
		if("；".equals(name) || "∘".equals(name)) {
			return Value.function(this::compose);
		} else {
			return Value.super.slot(self, name, ranges, fallback);
		}
	}

	public Value compose(Value functionAfter) {
		return new FunctionComposition(functionAfter, this);
	}
}
