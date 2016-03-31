package banjo.value.kernel;

import banjo.eval.Fail;
import banjo.eval.FailWithMessage;
import banjo.expr.source.Operator;
import banjo.expr.token.StringLiteral;
import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import banjo.value.ValueToStringTrait;
import fj.data.Either;
import fj.data.Set;

public class KernelStringValue extends ValueToStringTrait implements Value {
    public final String str;
    public final Value trueValue;
    public final Value falseValue;

    public KernelStringValue(String str, Value trueValue, Value falseValue) {
        this.str = str;
        this.trueValue = trueValue;
        this.falseValue = falseValue;
    }

    private Value bool(boolean x) {
        return x ? trueValue : falseValue;
    }

    /**
     * Return a new kernel with with new text but the other metadata the same.
     */
    private Value derived(String str) {
        return new KernelStringValue(str, trueValue, falseValue);
    }

    private Value num(int n) {
        return new KernelNumberValue(n, trueValue, falseValue);
    }

    /**
     * Try and calculate a slot of this string
     */
    @Override
    public Value slot(Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
        if(name.equals(Operator.ABSVALUE.getMethodName()))
            return num(str.codePointCount(0, str.length()));
        
        if("kernel string value".equals(name))
            return this;

        if("label".equals(name))
            return derived(StringLiteral.toSource(str));

        if("all caps".equals(name))
            return derived(str.toUpperCase());
        if("none caps".equals(name))
            return derived(str.toLowerCase());
        
        if(name.equals(Operator.EQ.getMethodName()))
            return Value.function((Value s) -> {
                Value ss = s.slot("kernel string value");
                return bool((ss instanceof KernelStringValue) && ((KernelStringValue)ss).str.equals(str));
            });
        if(name.equals(Operator.LT.getMethodName()))
            return Value.function((Value s) -> {
                Value ss = s.slot("kernel string value");
                if(ss instanceof KernelStringValue)
                    return bool(((KernelStringValue)ss).str.compareTo(str) < 0);
                return new FailWithMessage("Can only compare kernel strings with kernel strings");
            });
        if(name.equals(Operator.ADD.getMethodName()))
            return Value.function((Value s) -> {
                Value ss = s.slot("kernel string value");
                if(ss instanceof KernelStringValue)
                    return derived(str + ((KernelStringValue)ss).str);
                return new FailWithMessage("Can only concatenate kernel strings with kernel strings");
            });
        
        return Value.super.slot(self, name, ranges, fallback);
    }

    @Override
    public <T> Either<T, Fail> convertToJava(Class<T> clazz) {
        if(clazz.isInstance(str)) {
            return Either.left(clazz.cast(str));
        }
        return Value.super.convertToJava(clazz);
    }

}
