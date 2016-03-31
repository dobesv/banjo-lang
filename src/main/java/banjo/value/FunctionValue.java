package banjo.value;

import java.util.function.Function;

import banjo.eval.ArgumentNotSupplied;
import banjo.eval.Fail;
import banjo.eval.util.JavaMethodCall;
import fj.data.Either;
import fj.data.List;

public class FunctionValue extends JavaObjectValue {

	public FunctionValue(Function<Value,Value> x) {
	    super(x);
    }

	@Override
	public <T> Either<T, Fail> convertToJava(Class<T> clazz) {
		if(clazz.isAssignableFrom(Function.class)) {
			return Either.left(clazz.cast(object));
		}
	    return super.convertToJava(clazz);
	}

	@Override
	public Value call(List<Value> arguments) {
		if(arguments.isEmpty()) {
			return new ArgumentNotSupplied("Missing argument to function");
		}
		return new JavaMethodCall(object, instanceMethodsWithName(object.getClass(), "apply"), arguments.take(1));
	}

	@Override
	public Value call(Value recurse, Value baseImpl, List<Value> arguments) {
		// No self-recursion and no previous implementation support for java methods
		return call(arguments);
	}
}
