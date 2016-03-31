package banjo.eval.util;

import java.util.function.Function;

import banjo.eval.ArgumentNotSupplied;
import banjo.eval.Fail;
import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import fj.data.Either;
import fj.data.List;
import fj.data.Set;

public class Selector implements Value, Function<Value, Value> {
	public final String slotName;
    public final Set<SourceFileRange> ranges;

    public Selector(String slotName, Set<SourceFileRange> ranges) {
		this.slotName = slotName;
        this.ranges = ranges;
	}

	@Override
	public String toString() {
	    return "."+slotName;
	}

	@Override
	public Value apply(Value target) {
		return target.slot(slotName, ranges);
	}

	@Override
	public Value call(List<Value> arguments) {
		return arguments.toOption().map(this::apply).orSome(() -> new ArgumentNotSupplied("Selector "+this+" called without arguments"));
	}

	@Override
	public Value call(Value recurse, Value baseImpl, List<Value> arguments) {
	    return call(arguments);
	}

	@Override
	public <T> Either<T, Fail> convertToJava(Class<T> clazz) {
		if(clazz.isAssignableFrom(Function.class)) {
			return Either.left(clazz.cast(this));
		}
		return Value.super.convertToJava(clazz);
	}
	
	@Override
	public String toStringFallback() {
		return "."+slotName;
	}
}