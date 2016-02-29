package banjo.value;

import banjo.eval.Fail;
import banjo.eval.util.JavaLanguageRuntimeImpl;
import banjo.event.PastEvent;
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
	final List<Value> stack = JavaLanguageRuntimeImpl.stack.get();
	public Value memo;

	@Override
    public Value call(Value recurse, Value baseImpl, List<Value> arguments) {
        return force().call(recurse, baseImpl, arguments);
    }

	@Override
    public Value callMethod(String name, Set<SourceFileRange> ranges, Value targetObject, Value fallback, List<Value> args) {
    	return force().callMethod(name, ranges, targetObject, fallback, args);
    }

	@Override
    public Value slot(Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
    	return force().slot(self, name, ranges, fallback);
    }

	@Override
	public Value slot(String name, Set<SourceFileRange> ranges) {
	    return force().slot(name, ranges);
	}

	@Override
	public boolean isTruthy() {
	    return force().isTruthy();
	}

	@Override
	public Value force() {
        if(this.memo == null) {
            List<Value> oldStack = JavaLanguageRuntimeImpl.setStack(this.stack);
            try {
                Value value = calculate();
                while((value instanceof CalculatedValue) && !((CalculatedValue) value).isCalculationReactive())
                    value = ((CalculatedValue) value).force();
                this.memo = value;
            } finally {
                JavaLanguageRuntimeImpl.stack.set(oldStack);
            }
        }
        return memo.force();
    }

	@Override
	public boolean isDefined() {
	    return force().isDefined();
    }

	@Override
	public <T> Either<T, Fail> convertToJava(Class<T> clazz) {
	    Value value = force();
	    if(value == this)
	    	throw new Error();
		return value.convertToJava(clazz);
    }

	@Override
	public String javaLabel() {
	    return force().javaLabel();
    }

	@Override
	public String toStringFallback() {
	    return force().toStringFallback();
    }

	@Override
	public Reaction<Value> react(PastEvent event) {
		if(this.memo == null)
			return calculationReact(event);
		else
			return memo.react(event);
	}
	
	@Override
	public boolean isReactive() {
		if(this.memo == null)
			return isCalculationReactive();
		else
			return memo.isReactive();
	}

	public abstract Value calculate();
	
	/**
	 * Determine whether the result of this lazy operation depends on a reactive value,
	 * without calculating it.
	 */
	public abstract boolean isCalculationReactive();
	
	/**
	 * Have the calculation result react to an event, without actually calculating it.
	 */
	public abstract Reaction<Value> calculationReact(PastEvent event);

}
