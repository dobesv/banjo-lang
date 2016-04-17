package banjo.value;

import banjo.eval.Fail;
import banjo.expr.util.SourceFileRange;
import fj.data.Either;
import fj.data.List;
import fj.data.Set;

/**
 * Represents a value that is calculated from some dependent values.  This
 * not is used as a "thunk" for lazy values and for reactive values that
 * change in response to events. 
 */
public abstract class CalculatedValue extends ValueToStringTrait implements Value {
	public Value memo;

	@Override
    public Value call(List<Value> trace, Value recurse, Value baseImpl, List<Value> arguments) {
        return force(trace).call(trace, recurse, baseImpl, arguments);
    }

	@Override
    public Value callMethod(List<Value> trace, String name, Set<SourceFileRange> ranges, Value targetObject, Value fallback, List<Value> args) {
        return force(trace).callMethod(trace, name, ranges, targetObject, fallback, args);
    }

	@Override
    public Value slot(List<Value> trace, Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
        return force(trace).slot(trace, self, name, ranges, fallback);
    }

	@Override
	public Value slot(List<Value> trace, String name, Set<SourceFileRange> ranges) {
        return force(trace).slot(trace, name, ranges);
	}

	@Override
	public boolean isTrue(List<Value> trace) {
        return force(trace).isTrue(trace);
	}

	@Override
    public Value force(List<Value> trace) {
        if(this.memo == null) {
            this.memo = calculate(trace);
        }
        return memo;
    }

	@Override
	public boolean isDefined(List<Value> trace) {
        return force(trace).isDefined(trace);
    }

	@Override
	public <T> Either<T, Fail> convertToJava(List<Value> trace, Class<T> clazz) {
        Value value = force(trace);
	    if(value == this)
	    	throw new Error();
		return value.convertToJava(trace, clazz);
    }

	@Override
	public String javaLabel(List<Value> trace) {
        return force(trace).javaLabel(trace);
    }

	@Override
	public String toStringFallback(List<Value> trace) {
        return force(trace).toStringFallback(trace);
    }

    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return force(List.nil()).acceptVisitor(visitor);
    }

    /**
     * The subclass must implement the actual calculation for this value.
     * @param trace TODO
     */
	public abstract Value calculate(List<Value> trace);

}
