package banjo.value;

import banjo.eval.EvalContext;
import banjo.eval.expr.CallInstance;
import banjo.expr.util.SourceFileRange;
import banjo.value.fail.Fail;
import fj.data.Either;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;

/**
 * Represents a value that is calculated from some dependent values.  This
 * not is used as a "thunk" for lazy values and for reactive values that
 * change in response to events. 
 */
public abstract class CalculatedValue implements Value {
	public Value memo;
	
	@Override
    public Value call(EvalContext<Value> ctx, List<Value> arguments) {
        return new CallInstance(SourceFileRange.EMPTY_SET, force(ctx), arguments);
    }

    @Override
    public Value call(EvalContext<Value> ctx, Value recurse, Value baseImpl, List<Value> arguments) {
        return force(ctx).call(ctx, recurse, baseImpl, arguments);
    }

	@Override
    public Value callMethod(EvalContext<Value> ctx, String name, Set<SourceFileRange> ranges, Value targetObject, Value fallback, List<Value> args) {
        return force(ctx).callMethod(ctx, name, ranges, targetObject, fallback, args);
    }

	@Override
    public Value slot(EvalContext<Value> ctx, Value self, String slotName, Set<SourceFileRange> ranges,
            Option<Value> fallback) {
        Value target = force(ctx);
        return new SlotValue(target, self == this ? target : self, slotName, ranges, fallback);
    }

	@Override
	public Value slot(EvalContext<Value> ctx, String name, Set<SourceFileRange> ranges) {
        Value target = force(ctx);
        return new SlotValue(target, ranges, name);
	}

	@Override
	public boolean isTrue(EvalContext<Value> ctx) {
        return force(ctx).isTrue(ctx);
	}

	@Override
    public Value force(EvalContext<Value> ctx) {
        if(this.memo == null) {
            this.memo = calculate(ctx).force(ctx);
        }
        return memo;
    }

	@Override
	public boolean isDefined(EvalContext<Value> ctx) {
        return force(ctx).isDefined(ctx);
    }

	@Override
	public <T> Either<T, Fail> convertToJava(EvalContext<Value> ctx, Class<T> clazz) {
        Value value = force(ctx);
	    if(value == this)
	    	throw new Error();
        return value.convertToJava(ctx, clazz);
    }

	@Override
	public String javaLabel(EvalContext<Value> ctx) {
        return force(ctx).javaLabel(ctx);
    }

	@Override
	public String toStringFallback(EvalContext<Value> ctx) {
        return force(ctx).toStringFallback(ctx);
    }

    /**
     * The subclass must implement the actual calculation for this value.
     * @param ctx TODO
     */
	public abstract Value calculate(EvalContext<Value> ctx);

}
