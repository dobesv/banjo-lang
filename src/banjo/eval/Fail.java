package banjo.eval;

import java.util.function.Supplier;

import com.sun.javafx.binding.ObjectConstant;

import banjo.eval.util.JavaRuntimeSupport;
import banjo.event.PastEvent;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.data.Either;
import fj.data.List;
import javafx.beans.value.ObservableValue;


public class Fail implements Value {
	
	final List<Value> trace = JavaRuntimeSupport.stack.get();
	
	public Fail() {
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
	public Reaction<Value> react(PastEvent event) {
		return Reaction.of(this);
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
	
	@Override
	public ObservableValue<Value> toObservableValue() {
		return ObjectConstant.valueOf(this);
	}

	public String getMessage() {
		return getClass().getName();
	}

	public Throwable getCause() {
		return null;
	}

	public List<Value> getTrace() {
		return trace;
	}
	
	@Override
	public String toString() {
		return getClass().getSimpleName()+": "+getMessage();
	}
	
}