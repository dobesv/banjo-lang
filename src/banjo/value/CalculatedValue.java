package banjo.value;

import banjo.eval.Fail;
import banjo.eval.util.JavaRuntimeSupport;
import banjo.event.PastEvent;
import fj.data.Either;
import fj.data.List;

/**
 * Represents a value that is calculated from some dependent values.  This
 * not is used as a "thunk" for lazy values and for reactive values that
 * change in response to events. 
 */
public abstract class CalculatedValue extends ValueToStringTrait implements Value {
	final List<Value> stack = JavaRuntimeSupport.stack.get();
	public Value memo;

	@Override
    public Value call(Value recurse, Value baseImpl, List<Value> arguments) {
        return get().call(recurse, baseImpl, arguments);
    }

	@Override
    public Value callMethod(String name, Value targetObject, Value fallback, List<Value> args) {
    	return get().callMethod(name, targetObject, fallback, args);
    }

	@Override
    public Value slot(Value self, String name, Value fallback) {
    	return get().slot(self, name, fallback);
    }

	@Override
	public Value slot(String name) {
	    return get().slot(name);
	}

	@Override
	public boolean isTruthy() {
	    return get().isTruthy();
	}

	@Override
	public Value force() {
	    return get().force();
    }

	@Override
	public boolean isDefined() {
	    return get().isDefined();
    }

	@Override
	public <T> Either<T, Fail> convertToJava(Class<T> clazz) {
	    Value value = get();
	    if(value == this)
	    	throw new Error();
		return value.convertToJava(clazz);
    }

	@Override
	public String javaLabel() {
	    return get().javaLabel();
    }

	@Override
	public String toStringFallback() {
	    return get().toStringFallback();
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

	public Value get() {
		if(this.memo == null) {
			List<Value> oldStack = JavaRuntimeSupport.stack.get();
			try {
				Value value = calculate();
				while((value instanceof CalculatedValue) && !((CalculatedValue)value).isCalculationReactive())
					value = ((CalculatedValue)value).get();
				this.memo = value;
			} finally {
				JavaRuntimeSupport.stack.set(oldStack);
			}
		}
		return this.memo;
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
