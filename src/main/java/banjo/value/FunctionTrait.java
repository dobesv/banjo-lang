package banjo.value;

import java.util.Arrays;

import banjo.expr.source.Operator;
import banjo.expr.util.SourceFileRange;
import banjo.value.meta.FunctionComposition;
import fj.data.List;
import fj.data.Set;

/**
 * Supply slots for function composition
 */
public abstract class FunctionTrait extends ValueToStringTrait implements Value {


	@Override
	public Value slot(List<Value> trace, Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
        if(Arrays.asList(Operator.FUNCTION_COMPOSITION_LEFT.ops).contains(name)) {
			return Value.function(this::compose);
		} else {
			return Value.super.slot(trace, self, name, ranges, fallback);
		}
	}

	public Value compose(Value functionAfter) {
		return new FunctionComposition(functionAfter, this);
	}

}
