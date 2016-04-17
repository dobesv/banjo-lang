package banjo.value.kernel;

import java.util.function.BiFunction;

import banjo.eval.FailWithMessage;
import banjo.expr.source.Operator;
import banjo.value.BaseValueVisitor;
import banjo.value.Value;
import banjo.value.ValueVisitor;
import fj.data.List;
import fj.data.Option;

/**
 * Wrapper for a boolean coming from java.
 */
public class KernelBooleanValue extends KernelValueWrapper<Boolean> implements Value {

    public KernelBooleanValue(Boolean value, Value trueValue, Value falseValue) {
        super(value, trueValue, falseValue);
    }

    public static Option<Boolean> extractBoolean(List<Value> trace, Value v) {
        return v.slot(trace, "kernel boolean value").acceptVisitor(new BaseValueVisitor<Option<Boolean>>() {
            @Override
            public Option<Boolean> fallback() {
                return Option.none();
            }

            @Override
            public Option<Boolean> kernelBoolean(KernelBooleanValue kernelBooleanValue) {
                return Option.some(kernelBooleanValue.value);
            }
        });
    }

    public Value callBinaryOp(List<Value> trace, BiFunction<Boolean, Boolean, Value> op, Value operand, String operation) {
        return extractBoolean(trace, operand)
            .map(b -> op.apply(this.value, b))
            .orSome(() -> new FailWithMessage(trace, "Can only " + operation + " kernel booleans with kernel booleans"));
    }

    public Value binaryOp(List<Value> trace, BiFunction<Boolean, Boolean, Value> op, String operation) {
        return Value.function((Value operand) -> callBinaryOp(trace, op, operand, operation));
    }

    public Value booleanBinaryOp(List<Value> trace, BiFunction<Boolean, Boolean, Boolean> op, String operation) {
        return binaryOp(trace, op.andThen(this::boolValue), operation);
    }

    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.kernelBoolean(this);
    }

    @Override
    public Value slot(List<Value> trace, String name) {
        if("kernel boolean value".equals(name)) {
            return this;
        }
        if("if".equals(name))
            return Value.function(v -> v.slot(trace, value.booleanValue() ? "true" : "false"));
        if(Operator.EQ.getMethodName().equals(name))
            return booleanBinaryOp(trace, (a, b) -> a.booleanValue() == b.booleanValue(), "compare");
        if(Operator.LT.getMethodName().equals(name))
            return booleanBinaryOp(trace, (a, b) -> Boolean.compare(a.booleanValue(), b.booleanValue()) < 0, "compare");

        // true && x, false || x both yield x
        if((value.booleanValue() && Operator.LOGICAL_AND.getMethodName().equals(name)) ||
            (!value.booleanValue() && Operator.LOGICAL_OR.getMethodName().equals(name)))
            return Value.IDENTITY_FUNCTION;

        // true || x, false && x yields the same object
        if((!value.booleanValue() && Operator.LOGICAL_AND.getMethodName().equals(name)) ||
            (value.booleanValue() && Operator.LOGICAL_OR.getMethodName().equals(name)))
            return Value.function((a) -> this);

        if(Operator.NOT.getMethodName().equals(name))
            return boolValue(!value.booleanValue());

        return Value.super.slot(trace, name);
    }
}
