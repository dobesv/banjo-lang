package banjo.value.special;

import banjo.eval.EvalContext;
import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import banjo.value.fail.Fail;
import fj.data.Either;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;

/**
 * Utility base class that wraps another object
 * and delegates all "Value" operations to that
 * object.
 *
 * A subclass can then override just the operations
 * that it needs to.
 */
public abstract class WrapperValue implements Value {

	public final Value target;

	public WrapperValue(Value target) {
		this.target = target;
    }

	@Override
    public Value call(EvalContext<Value> ctx, Value recurse, Value baseImpl, List<Value> arguments) {
        return target.call(ctx, recurse, baseImpl, arguments);
    }

	@Override
    public Value callMethod(EvalContext<Value> ctx, String name, Set<SourceFileRange> ranges, Value targetObject, Value fallback, List<Value> args) {
        return target.callMethod(ctx, name, ranges, targetObject, fallback, args);
    }

	@Override
    public Value slot(EvalContext<Value> ctx, Value self, String name, Set<SourceFileRange> ranges, Option<Value> fallback) {
    	return target.slot(ctx, self, name, ranges, fallback);
    }

	@Override
    public Value force(EvalContext<Value> ctx) {
        return target.force(ctx);
    }

	@Override
	public boolean isDefined(EvalContext<Value> ctx) {
	    return target.isDefined(ctx);
    }

	@Override
	public <T> Either<T, Fail> convertToJava(EvalContext<Value> ctx, Class<T> clazz) {
        return target.convertToJava(ctx, clazz);
    }

	@Override
	public String javaLabel(EvalContext<Value> ctx) {
        return target.javaLabel(ctx);
    }

	@Override
	public String toString() {
	    return target.toString();
	}

	public WrapperValue update(Value newValue) {
		if(newValue == target)
			return this;
		return this.rewrap(newValue);
	}
	
	protected abstract WrapperValue rewrap(Value newValue);
	
}