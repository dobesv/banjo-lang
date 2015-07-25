package banjo.eval;

import banjo.eval.util.JavaRuntimeSupport;
import banjo.event.Event;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.data.Either;
import fj.data.List;


public class Fail extends Error implements Value {
	@Override
	public synchronized Throwable fillInStackTrace() {
		Throwable t = super.fillInStackTrace();
		final StackTraceElement[] trace = Fail.makeTrace();
		if(trace.length > 0)
			t.setStackTrace(trace);
		return t;
	}

	public static StackTraceElement[] makeTrace() {
		List<StackTraceElement> trace = JavaRuntimeSupport.stack.get().map(x -> x.get());
		return trace.array(StackTraceElement[].class);
	}

	public Fail() {
	}

	public Fail(String message, Throwable cause) {
        super(message, cause);
    }

	public Fail(String message) {
        super(message);
    }

	public Fail(Throwable cause) {
        super(cause);
    }

	@Override
	public Value slot(String name) {
		return this;
	}

	@Override
	public Value slot(Value self, String name, Value fallback) {
	    return this;
	}

	@Override
	public Value call(List<Value> arguments) {
	    return this;
	}

	@Override
	public Value call(Value recurse, Value baseImpl, List<Value> arguments) {
	    return this;
	}

	@Override
	public Value callMethod(String name, Value targetObject, Value fallback,
	        List<Value> args) {
	    return this;
	}

	@Override
	public Value force() {
	    return this;
	}

	@Override
	public boolean isDefined() {
		return false;
	}

	@Override
	public boolean isTruthy() {
	    return false;
	}

	@Override
	public Reaction<Value> react(Event event) {
		return Reaction.none(this);
	}
	
	@Override
	public boolean isReactive() {
		return false;
	}
	
	@Override
	public <T> Either<T, Fail> convertToJava(Class<T> clazz) {
		if(clazz.isAssignableFrom(Fail.class)) {
			return Either.left(clazz.cast(this));
		}
		return Either.right(this);
	}

	@Override
	public String javaLabel() {
		return this.toString();
	}
}