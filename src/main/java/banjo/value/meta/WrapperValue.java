package banjo.value.meta;

import banjo.eval.Fail;
import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import fj.data.Either;
import fj.data.List;
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
    public Value call(List<Value> trace, Value recurse, Value baseImpl, List<Value> arguments) {
        return target.call(trace, recurse, baseImpl, arguments);
    }

	@Override
    public Value callMethod(List<Value> trace, String name, Set<SourceFileRange> ranges, Value targetObject, Value fallback, List<Value> args) {
    	return target.callMethod(trace, name, ranges, targetObject, fallback, args);
    }

	@Override
    public Value slot(List<Value> trace, Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
    	return target.slot(trace, self, name, ranges, fallback);
    }

	@Override
    public Value force(List<Value> trace) {
        return target.force(trace);
    }

	@Override
	public boolean isDefined(List<Value> trace) {
	    return target.isDefined(trace);
    }

	@Override
	public <T> Either<T, Fail> convertToJava(List<Value> trace, Class<T> clazz) {
	    return target.convertToJava(trace, clazz);
    }

	@Override
	public String javaLabel(List<Value> trace) {
	    return target.javaLabel(trace);
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