package banjo.value.kernel;

import java.util.function.BiFunction;
import java.util.function.Function;

import banjo.value.Value;
import banjo.value.ValueVisitor;
import banjo.value.fail.ArgumentNotSupplied;
import banjo.value.fail.Fail;
import banjo.value.fail.FailWithMessage;
import fj.data.Either;
import fj.data.List;

public class KernelBiFunctionValue implements Value {

    private BiFunction<Value, Value, Value> function;

    public KernelBiFunctionValue(BiFunction<Value, Value, Value> function) {
        this.function = function;
    }

	@Override
	public <T> Either<T, Fail> convertToJava(List<Value> trace, Class<T> clazz) {
		if(clazz.isAssignableFrom(Function.class)) {
            return Either.left(clazz.cast(function));
		}
        return Either.right(new FailWithMessage(trace, "Cannot convert " + this + " to " + clazz));
	}

	@Override
	public Value call(List<Value> trace, List<Value> arguments) {
        if(arguments.isEmpty() || arguments.tail().isEmpty()) {
            return new ArgumentNotSupplied(trace, "Missing argument to function");
		}
        // return new JavaBiFunctionCallResultValue(function, arguments.head(),
        // arguments.tail().head());
        return function.apply(arguments.head(), arguments.tail().head());
	}

	@Override
	public Value call(List<Value> trace, Value recurse, Value baseImpl, List<Value> arguments) {
		// No self-recursion and no previous implementation support for java methods
		return call(trace, arguments);
	}

    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.kernelBiFunction(this);
    }
}
