package banjo.eval.util;

import java.util.function.Function;

import banjo.eval.EvalContext;
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
    public final EvalContext<Value> ctx;

    public Selector(EvalContext<Value> ctx, String slotName, Set<SourceFileRange> ranges) {
		this.slotName = slotName;
        this.ranges = ranges;
        this.ctx = ctx;
	}

	@Override
	public String toString() {
	    return "."+slotName;
	}

	@Override
	public Value apply(Value target) {
        return target.slot(ctx, slotName, ranges);
	}

	@Override
	public Value call(EvalContext<Value> ctx, List<Value> arguments) {
        return arguments.headOption().map(this::apply)
                .orSome(() -> new ArgumentNotSupplied(ctx, "Selector " + this + " called without argument"));
	}

	@Override
	public Value call(EvalContext<Value> ctx, Value recurse, Value baseImpl, List<Value> arguments) {
	    return call(ctx, arguments);
	}

	@Override
	public <T> Either<T, Fail> convertToJava(EvalContext<Value> ctx, Class<T> clazz) {
		if(clazz.isAssignableFrom(Function.class)) {
			return Either.left(clazz.cast(this));
		}
        return Value.super.convertToJava(ctx, clazz);
	}
	
	@Override
	public String toStringFallback(EvalContext<Value> ctx) {
		return "."+slotName;
	}

    @Override
	public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.selector(this);
	}
}