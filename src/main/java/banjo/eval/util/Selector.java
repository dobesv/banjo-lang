package banjo.eval.util;

import java.util.function.Function;

import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import banjo.value.ValueVisitor;
import banjo.value.fail.ArgumentNotSupplied;
import banjo.value.fail.Fail;
import fj.data.Either;
import fj.data.List;
import fj.data.Set;

public class Selector implements Value, Function<Value, Value> {
	public final String slotName;
    public final Set<SourceFileRange> ranges;
    public final List<Value> trace;

    public Selector(List<Value> trace, String slotName, Set<SourceFileRange> ranges) {
		this.slotName = slotName;
        this.ranges = ranges;
        this.trace = trace;
	}

	@Override
	public String toString() {
	    return "."+slotName;
	}

	@Override
	public Value apply(Value target) {
		return target.slot(trace, slotName, ranges);
	}

	@Override
	public Value call(List<Value> trace, List<Value> arguments) {
        return arguments.toOption().map(this::apply).orSome(() -> new ArgumentNotSupplied(trace, "Selector " + this + " called without argument"));
	}

	@Override
	public Value call(List<Value> trace, Value recurse, Value baseImpl, List<Value> arguments) {
	    return call(trace, arguments);
	}

	@Override
	public <T> Either<T, Fail> convertToJava(List<Value> trace, Class<T> clazz) {
		if(clazz.isAssignableFrom(Function.class)) {
			return Either.left(clazz.cast(this));
		}
		return Value.super.convertToJava(trace, clazz);
	}
	
	@Override
	public String toStringFallback(List<Value> trace) {
		return "."+slotName;
	}

    @Override
	public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.kernelFunction(Value.function(this));
	}
}