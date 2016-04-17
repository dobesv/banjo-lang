package banjo.value.kernel;

import java.util.Objects;
import java.util.function.BiFunction;

import banjo.eval.FailWithMessage;
import banjo.expr.source.Operator;
import banjo.expr.token.StringLiteral;
import banjo.expr.util.SourceFileRange;
import banjo.value.BaseValueVisitor;
import banjo.value.Value;
import banjo.value.ValueVisitor;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;

public class KernelStringValue extends KernelValueWrapper<String> implements Value {
    public KernelStringValue(String str, Value trueValue, Value falseValue) {
        super(str, trueValue, falseValue);
    }

    /**
     * Return a new kernel with with new text but the other metadata the same.
     */
    private Value derived(String str) {
        return new KernelStringValue(str, trueValue, falseValue);
    }

    public static Option<String> extractString(List<Value> trace, Value v) {
        return v.slot(trace, "kernel string value").acceptVisitor(new BaseValueVisitor<Option<String>>() {
            @Override
            public Option<String> fallback() {
                return Option.none();
            }

            @Override
            public Option<String> kernelString(KernelStringValue kernelStringValue) {
                return Option.some(kernelStringValue.value);
            }
        });
    }

    public Value callBinaryOp(List<Value> trace, BiFunction<String, String, Value> op, Value operand, String operation) {
        return extractString(trace, operand)
            .map(b -> op.apply(this.value, b))
            .orSome(() -> new FailWithMessage(trace, "Can only " + operation + " kernel strings with kernel strings"));
    }

    public Value binaryOp(List<Value> trace, BiFunction<String,String,Value> op, String operation) {
        return Value.function((Value operand) -> callBinaryOp(trace, op, operand, operation));
    }

    public Value booleanBinaryOp(List<Value> trace, BiFunction<String, String, Boolean> op, String operation) {
        return binaryOp(trace, op.andThen(this::boolValue), operation);
    }

    public Value stringBinaryOp(List<Value> trace, BiFunction<String, String, String> op, String operation) {
        return binaryOp(trace, op.andThen(this::derived), operation);
    }

    /**
     * Try and calculate a slot of this string
     */
    @Override
    public Value slot(List<Value> trace, Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
        if(name.equals(Operator.ABSVALUE.getMethodName()))
            return intValue(value.codePointCount(0, value.length()));
        
        if("kernel string value".equals(name))
            return this;

        if("label".equals(name))
            return derived(StringLiteral.toSource(value));

        if("all caps".equals(name))
            return derived(value.toUpperCase());

        if("none caps".equals(name))
            return derived(value.toLowerCase());
        
        if(name.equals(Operator.EQ.getMethodName()))
            return booleanBinaryOp(trace, Objects::equals, "compare");
        if(name.equals(Operator.LT.getMethodName()))
            return booleanBinaryOp(trace, (String a, String b) -> a.compareTo(b) < 0, "compare");
        if(name.equals(Operator.ADD.getMethodName()))
            return stringBinaryOp(trace, (String a, String b) -> a + b, "concatenate");

        return Value.super.slot(trace, self, name, ranges, fallback);
    }

    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.kernelString(this);
    }
}
