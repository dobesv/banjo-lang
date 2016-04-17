package banjo.value;

import java.util.function.Function;

import banjo.eval.ArgumentNotSupplied;
import banjo.eval.Fail;
import banjo.eval.FailWithMessage;
import fj.data.Either;
import fj.data.List;

public class KernelFunctionValue implements Value {

    private Function<Value, Value> function;

    public KernelFunctionValue(Function<Value, Value> function) {
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
		if(arguments.isEmpty()) {
            return new ArgumentNotSupplied(trace, "Function " + this + "expects 1 argument; got none");
		}
        // return new JavaFunctionCallResultValue(function, arguments.head());
        return function.apply(arguments.head());
	}

	@Override
	public Value call(List<Value> trace, Value recurse, Value baseImpl, List<Value> arguments) {
		// No self-recursion and no previous implementation support for java methods
		return call(trace, arguments);
	}

    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.kernelFunction(this);
    }
}
