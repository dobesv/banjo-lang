package banjo.value;

import java.util.function.Supplier;

import banjo.eval.Fail;
import banjo.event.Event;
import fj.data.Either;
import fj.data.List;

public class CalculatedValue extends ValueToStringTrait implements Value {
	public final Supplier<Value> calculation;

	public CalculatedValue(Supplier<Value> calculation) {
	    super();
	    this.calculation = calculation;
    }

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
	    return get().convertToJava(clazz);
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
	public Reaction<Value> react(Event event) {
		return get().react(event);
	}

	public Value get() {
	    final Value value = calculation.get();
	    if(value == null)
	    	return new Fail("null value - probably a self-referential calculation");
		return value;
    }
}
