package banjo.value.kernel;

import banjo.eval.Fail;
import banjo.value.Value;
import banjo.value.ValueToStringTrait;
import fj.data.Either;
import fj.data.List;

public abstract class KernelValueWrapper<T> extends ValueToStringTrait implements Value {
    public final T value;
    public final Value trueValue;
    public final Value falseValue;
    
    public KernelValueWrapper(T value, Value trueValue, Value falseValue) {
        super();
        this.value = value;
        this.trueValue = trueValue;
        this.falseValue = falseValue;
    }

    @Override
    public String toString() {
        return value.toString();
    }

    public Value boolValue(boolean x) {
        return x ? trueValue : falseValue;
    }

    public Value intValue(int n) {
        return new KernelNumberValue(n, trueValue, falseValue);
    }

    public Value valueIsInstanceOf(Class<? extends T> cls) {
        return boolValue(cls.isInstance(this.value));
    }

    @Override
    public <T> Either<T, Fail> convertToJava(List<Value> trace, Class<T> clazz) {
        if(clazz.isInstance(value)) {
            return Either.left(clazz.cast(value));
        }
        return Value.super.convertToJava(trace, clazz);
    }
}
