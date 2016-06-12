package banjo.value.kernel;

import banjo.expr.source.Operator;
import banjo.value.Value;
import banjo.value.ValueToStringTrait;
import banjo.value.fail.Fail;
import fj.data.Either;
import fj.data.List;

public abstract class KernelValueWrapper<T> extends ValueToStringTrait implements Value {
    public final T value;
    public final Value trueValue;
    
    public KernelValueWrapper(T value, Value trueValue) {
        super();
        this.value = value;
        this.trueValue = trueValue;
    }

    @Override
    public String toString() {
        return value.toString();
    }

    public Value boolValue(boolean x) {
        // No trace on the "not" - we're assuming for now that "true" is working
        // properly and doesn't need any debugging
        return x ? trueValue : trueValue.slot(List.nil(), Operator.NOT.methodName);
    }

    public Value intValue(int n) {
        return new KernelNumberValue(n, trueValue);
    }

    public Value valueIsInstanceOf(Class<? extends T> cls) {
        return boolValue(cls.isInstance(this.value));
    }

    @Override
    public <JT> Either<JT, Fail> convertToJava(List<Value> trace, Class<JT> clazz) {
        if(clazz.isInstance(value)) {
            return Either.left(clazz.cast(value));
        }
        return Value.super.convertToJava(trace, clazz);
    }
}
