package banjo.value.meta;

import banjo.eval.Fail;
import banjo.event.Event;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.data.Either;
import fj.data.List;

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
    public Value call(Value recurse, Value baseImpl, List<Value> arguments) {
        return target.call(recurse, baseImpl, arguments);
    }

	@Override
    public Value callMethod(String name, Value targetObject, Value fallback, List<Value> args) {
    	return target.callMethod(name, targetObject, fallback, args);
    }

	@Override
    public Value slot(Value self, String name, Value fallback) {
    	return target.slot(self, name, fallback);
    }

	@Override
	public Value force() {
	    return target.force();
    }

	@Override
	public boolean isDefined() {
	    return target.isDefined();
    }

	@Override
	public <T> Either<T, Fail> convertToJava(Class<T> clazz) {
	    return target.convertToJava(clazz);
    }

	@Override
	public String javaLabel() {
	    return target.javaLabel();
    }

	@Override
	public String toString() {
	    return target.toString();
	}

	@Override
	public Reaction<Value> react(Event event) {
		return target.react(event).map(this::update);
	}

	@Override
	public boolean isReactive() {
		return target.isReactive();
	}
	
	public Value update(Value newValue) {
		if(newValue == target)
			return this;
		return this.rewrap(newValue);
	}
	
	protected abstract Value rewrap(Value newValue);

}