package banjo.value.kernel;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

import org.apache.commons.math3.fraction.BigFraction;

import banjo.eval.Fail;
import banjo.eval.FailWithMessage;
import banjo.expr.source.Operator;
import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import banjo.value.ValueToStringTrait;
import fj.data.Either;
import fj.data.Set;

public class KernelNumberValue extends ValueToStringTrait implements Value {

    public final Number number;
    public final Value trueValue;
    public final Value falseValue;

    public KernelNumberValue(Number number, Value trueValue, Value falseValue) {
        this.number = number;
        this.trueValue = trueValue;
        this.falseValue = falseValue;
    }

    @Override
    public String toString() {
        return number.toString();
    }

    private Value bool(boolean x) {
        return x ? trueValue : falseValue;
    }

    private Value numberIsInstanceOf(Class<? extends Number> cls) {
        return bool(cls.isInstance(this.number));
    }

    /**
     * Return a new KernelNumberValue with a (probably) different number but the
     * same reference to true and false that we need for the boolean slot
     * implementations.
     */
    private Value derived(Number n) {
        return new KernelNumberValue(n, trueValue, falseValue);
    }

    private static Number extractNumber(Value v) {
        Value kernelNumberValue = v.slot("kernel number value").force();
        if(!(kernelNumberValue instanceof KernelNumberValue)) {
            return Double.NaN;
        }
        return ((KernelNumberValue)kernelNumberValue).number;
    }

    /**
     * Convert some Number value to BigInteger.
     */
    public static BigInteger bigIntegerValue(Number number) {
        if(number instanceof BigInteger) {
            return (BigInteger) number;
        } else if(number instanceof Long || number instanceof Integer || number instanceof Short || number instanceof Byte) {
            return BigInteger.valueOf(number.longValue());
        } else {
            return new BigInteger(number.toString());
        }
    }

    /**
     * Convert some Number value to BigDecimal.
     */
    public static BigDecimal bigDecimalValue(Number number) {
        if(number instanceof BigDecimal) {
            return (BigDecimal) number;
        } else if(number instanceof BigInteger) {
            return new BigDecimal((BigInteger) number);
        } else if(number instanceof Long || number instanceof Integer || number instanceof Short || number instanceof Byte) {
            return BigDecimal.valueOf(number.longValue());
        } else if(number instanceof BigFraction) {
            return ((BigFraction) number).bigDecimalValue();
        } else {
            return new BigDecimal(number.toString());
        }
    }

    public static BigFraction bigDecimalToBigFraction(BigDecimal bd) {
        int scale = bd.scale();

        // If scale >= 0 then the value is bd.unscaledValue() / 10^scale
        if(scale >= 0)
            return new BigFraction(bd.unscaledValue(), BigInteger.TEN.pow(scale));
        // If scale < 0 then the value is bd.unscaledValue() * 10^-scale
        return new BigFraction(bd.unscaledValue().multiply(BigInteger.TEN.pow(-scale)));
    }

    /**
     * Convert some Number value to BigFraction.
     */
    public static BigFraction bigFractionValue(Number number) {
        if(number instanceof BigFraction) {
            return (BigFraction) number;
        } else if(number instanceof BigInteger) {
            return new BigFraction((BigInteger) number);
        } else if(number instanceof Long || number instanceof Integer || number instanceof Short || number instanceof Byte) {
            return new BigFraction(number.longValue());
        } else if(number instanceof Double || number instanceof Float) {
            return new BigFraction(number.doubleValue());
        } else if(number instanceof BigDecimal) {
            return bigDecimalToBigFraction((BigDecimal) number);
        } else {
            return new BigFraction(bigIntegerValue(number));
        }
    }

    /**
     * Try and calculate a slot of this number
     */
    @Override
    public Value slot(Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
        // Retrieve raw kernel number
        if("kernel number value".equals(name)) {
            return this;
        }
        
        // Type checks
        if("is int8".equals(name)) {
            return numberIsInstanceOf(Byte.class);
        } else if("is int16".equals(name)) {
            return numberIsInstanceOf(Short.class);
        } else if("is int32".equals(name)) {
            return numberIsInstanceOf(Integer.class);
        } else if("is int64".equals(name)) {
            return numberIsInstanceOf(Long.class);
        } else if("is integer".equals(name)) {
            return numberIsInstanceOf(BigInteger.class);
        } else if("is float32".equals(name)) {
            return numberIsInstanceOf(Float.class);            
        } else if("is float64".equals(name)) {
            return numberIsInstanceOf(Double.class);
        } else if("is decimal".equals(name)) {
            return numberIsInstanceOf(BigDecimal.class);
        } else if("is fraction".equals(name)) {
            return numberIsInstanceOf(BigFraction.class);
        }
            
        // Conversions
        double d = number.doubleValue();
        if("int8".equals(name)) {
            if(number instanceof Byte) {
                return this;
            } else {
                return derived(number.byteValue());
            }
        } else if("int16".equals(name)) {
            if(number instanceof Short) {
                return this;
            } else {
                return derived(number.shortValue());
            }
        } else if("int32".equals(name)) {
            if(number instanceof Integer) {
                return this;
            } else {
                return derived(number.intValue());
            }
        } else if("int64".equals(name)) {
            if(number instanceof Long) {
                return this;
            } else {
                return derived(number.longValue());
            }
        } else if("integer".equals(name)) {
            if(number instanceof BigInteger) {
                return this;
            } else {
                return derived(bigIntegerValue(number));
            }
        } else if("float32".equals(name)) {
            if(number instanceof Float) {
                return this;
            } else {
                return derived(number.floatValue());
            }
        } else if("float64".equals(name)) {
            if(number instanceof Double) {
                return this;
            } else {
                return derived(d);
            }
        } else if("decimal".equals(name)) {
            if(number instanceof BigDecimal) {
                return this;
            } else {
                return derived(bigDecimalValue(number));
            }
            
        } else if("fraction".equals(name)) {
            if(number instanceof BigFraction) {
                return this;
            } else {
                return derived(bigFractionValue(number));
            }
        }

        // Binary operators
        if(Operator.ADD.getMethodName().equals(name)) {
            if(number instanceof Byte) {
                return Value.function((Value v) -> derived(number.byteValue() + extractNumber(v).byteValue()));
            } else if(number instanceof Short) {
                return Value.function((Value v) -> derived(number.shortValue() + extractNumber(v).shortValue()));
            } else if(number instanceof Integer) {
                return Value.function((Value v) -> derived(number.intValue() + extractNumber(v).intValue()));
            } else if(number instanceof Long) {
                return Value.function((Value v) -> derived(number.longValue() + extractNumber(v).longValue()));
            } else if(number instanceof BigInteger) {
                return Value.function((Value v) -> derived(((BigInteger) number).add(bigIntegerValue(extractNumber(v)))));
            } else if(number instanceof Float) {
                return Value.function((Value v) -> derived(number.floatValue() + extractNumber(v).floatValue()));
            } else if(number instanceof Double) {
                return Value.function((Value v) -> derived(d + extractNumber(v).doubleValue()));
            } else if(number instanceof BigDecimal) {
                return Value.function((Value v) -> derived(((BigDecimal) number).add(bigDecimalValue(extractNumber(v)))));
            } else if(number instanceof BigFraction) {
                return Value.function((Value v) -> derived(((BigFraction) number).add(bigFractionValue(extractNumber(v)))));
            } else {
                return new FailWithMessage(name + " not implemented for number type: " + number.getClass().getSimpleName());
            }
        } else if(Operator.SUB.getMethodName().equals(name)) {
            if(number instanceof Byte) {
                return Value.function((Value v) -> derived(number.byteValue() - extractNumber(v).byteValue()));
            } else if(number instanceof Short) {
                return Value.function((Value v) -> derived(number.shortValue() - extractNumber(v).shortValue()));
            } else if(number instanceof Integer) {
                return Value.function((Value v) -> derived(number.intValue() - extractNumber(v).intValue()));
            } else if(number instanceof Long) {
                return Value.function((Value v) -> derived(number.longValue() - extractNumber(v).longValue()));
            } else if(number instanceof BigInteger) {
                return Value.function((Value v) -> derived(((BigInteger) number).subtract(bigIntegerValue(extractNumber(v)))));
            } else if(number instanceof Float) {
                return Value.function((Value v) -> derived(number.floatValue() - extractNumber(v).floatValue()));
            } else if(number instanceof Double) {
                return Value.function((Value v) -> derived(d - extractNumber(v).doubleValue()));
            } else if(number instanceof BigDecimal) {
                return Value.function((Value v) -> derived(((BigDecimal) number).subtract(bigDecimalValue(extractNumber(v)))));
            } else if(number instanceof BigFraction) {
                return Value.function((Value v) -> derived(((BigFraction) number).subtract(bigFractionValue(extractNumber(v)))));
            } else {
                return new FailWithMessage(name + " not implemented for number type: " + number.getClass().getSimpleName());
            }
        } else if(Operator.MUL.getMethodName().equals(name)) {
            if(number instanceof Byte) {
                return Value.function((Value v) -> derived(number.byteValue() * extractNumber(v).byteValue()));
            } else if(number instanceof Short) {
                return Value.function((Value v) -> derived(number.shortValue() * extractNumber(v).shortValue()));
            } else if(number instanceof Integer) {
                return Value.function((Value v) -> derived(number.intValue() * extractNumber(v).intValue()));
            } else if(number instanceof Long) {
                return Value.function((Value v) -> derived(number.longValue() * extractNumber(v).longValue()));
            } else if(number instanceof BigInteger) {
                return Value.function((Value v) -> derived(((BigInteger) number).multiply(bigIntegerValue(extractNumber(v)))));
            } else if(number instanceof Float) {
                return Value.function((Value v) -> derived(number.floatValue() * extractNumber(v).floatValue()));
            } else if(number instanceof Double) {
                return Value.function((Value v) -> derived(d * extractNumber(v).doubleValue()));
            } else if(number instanceof BigDecimal) {
                return Value.function((Value v) -> derived(((BigDecimal) number).multiply(bigDecimalValue(extractNumber(v)))));
            } else if(number instanceof BigFraction) {
                return Value.function((Value v) -> derived(((BigFraction) number).multiply(bigFractionValue(extractNumber(v)))));
            } else {
                return new FailWithMessage(name + " not implemented for number type: " + number.getClass().getSimpleName());
            }
        } else if(Operator.DIV.getMethodName().equals(name)) {
            if(number instanceof Byte) {
                return Value.function((Value v) -> derived(number.byteValue() / extractNumber(v).byteValue()));
            } else if(number instanceof Short) {
                return Value.function((Value v) -> derived(number.shortValue() / extractNumber(v).shortValue()));
            } else if(number instanceof Integer) {
                return Value.function((Value v) -> derived(number.intValue() / extractNumber(v).intValue()));
            } else if(number instanceof Long) {
                return Value.function((Value v) -> derived(number.longValue() / extractNumber(v).longValue()));
            } else if(number instanceof BigInteger) {
                return Value.function((Value v) -> derived(((BigInteger) number).divide(bigIntegerValue(extractNumber(v)))));
            } else if(number instanceof Float) {
                return Value.function((Value v) -> derived(number.floatValue() / extractNumber(v).floatValue()));
            } else if(number instanceof Double) {
                return Value.function((Value v) -> derived(d / extractNumber(v).doubleValue()));
            } else if(number instanceof BigDecimal) {
                return Value.function((Value v) -> derived(((BigDecimal) number).divide(bigDecimalValue(extractNumber(v)))));
            } else if(number instanceof BigFraction) {
                return Value.function((Value v) -> derived(((BigFraction) number).divide(bigFractionValue(extractNumber(v)))));
            } else {
                return new FailWithMessage(name + " not implemented for number type: " + number.getClass().getSimpleName());
            }
        } else if("bit shifted".equals(name)) {
            if(number instanceof Byte) {
                return Value.function((Value v) -> derived(number.byteValue() << extractNumber(v).intValue()));
            } else if(number instanceof Short) {
                return Value.function((Value v) -> derived(number.shortValue() << extractNumber(v).intValue()));
            } else if(number instanceof Integer) {
                return Value.function((Value v) -> derived(number.intValue() << extractNumber(v).intValue()));
            } else if(number instanceof Long) {
                return Value.function((Value v) -> derived(number.longValue() << extractNumber(v).intValue()));
            } else if(number instanceof BigInteger) {
                return Value.function((Value v) -> derived(((BigInteger) number).shiftLeft(extractNumber(v).intValue())));
            } else if(number instanceof Float) {
                return Value.function((Value v) -> derived(number.intValue() << extractNumber(v).intValue()));
            } else if(number instanceof Double) {
                return Value.function((Value v) -> derived(number.longValue() << extractNumber(v).intValue()));
            } else if(number instanceof BigDecimal) {
                return Value.function((Value v) -> derived(((BigDecimal) number).toBigInteger().shiftLeft(extractNumber(v).intValue())));
            } else if(number instanceof BigFraction) {
                BigFraction bf = (BigFraction) number;
                return Value.function((Value v) -> derived(bf.getNumerator().divide(bf.getDenominator()).shiftLeft(extractNumber(v).intValue())));
            } else {
                return new FailWithMessage(name + " not implemented for number type: " + number.getClass().getSimpleName());
            }
        } else if(Operator.EQ.getMethodName().equals(name)) {
            if(number instanceof Byte) {
                return Value.function((Value v) -> bool(number.byteValue() == extractNumber(v).byteValue()));
            } else if(number instanceof Short) {
                return Value.function((Value v) -> bool(number.shortValue() == extractNumber(v).shortValue()));
            } else if(number instanceof Integer) {
                return Value.function((Value v) -> bool(number.intValue() == extractNumber(v).intValue()));
            } else if(number instanceof Long) {
                return Value.function((Value v) -> bool(number.longValue() == extractNumber(v).longValue()));
            } else if(number instanceof BigInteger) {
                return Value.function((Value v) -> bool(((BigInteger) number).equals(bigIntegerValue(extractNumber(v)))));
            } else if(number instanceof Float) {
                return Value.function((Value v) -> bool(number.floatValue() == extractNumber(v).floatValue()));
            } else if(number instanceof Double) {
                return Value.function((Value v) -> bool(d == extractNumber(v).doubleValue()));
            } else if(number instanceof BigDecimal) {
                return Value.function((Value v) -> bool(((BigDecimal) number).compareTo(bigDecimalValue(extractNumber(v))) == 0));
            } else if(number instanceof BigFraction) {
                return Value.function((Value v) -> bool(((BigFraction) number).equals(bigFractionValue(extractNumber(v)))));
            } else {
                return new FailWithMessage(name + " not implemented for number type: " + number.getClass().getSimpleName());
            }
        } else if(Operator.LT.getMethodName().equals(name)) {
            if(number instanceof Byte) {
                return Value.function((Value v) -> bool(number.byteValue() < extractNumber(v).byteValue()));
            } else if(number instanceof Short) {
                return Value.function((Value v) -> bool(number.shortValue() < extractNumber(v).shortValue()));
            } else if(number instanceof Integer) {
                return Value.function((Value v) -> bool(number.intValue() < extractNumber(v).intValue()));
            } else if(number instanceof Long) {
                return Value.function((Value v) -> bool(number.longValue() < extractNumber(v).longValue()));
            } else if(number instanceof BigInteger) {
                return Value.function((Value v) -> bool(((BigInteger) number).compareTo(bigIntegerValue(extractNumber(v))) < 0));
            } else if(number instanceof Float) {
                return Value.function((Value v) -> bool(number.floatValue() < extractNumber(v).floatValue()));
            } else if(number instanceof Double) {
                return Value.function((Value v) -> bool(d < extractNumber(v).doubleValue()));
            } else if(number instanceof BigDecimal) {
                return Value.function((Value v) -> bool(((BigDecimal) number).compareTo(bigDecimalValue(extractNumber(v))) < 0));
            } else if(number instanceof BigFraction) {
                return Value.function((Value v) -> bool(((BigFraction) number).compareTo(bigFractionValue(extractNumber(v))) < 0));
            } else {
                return new FailWithMessage(name + " not implemented for number type: " + number.getClass().getSimpleName());
            }
        }

        // Unary operators
        if(Operator.NEGATE.getMethodName().equals(name)) {
            if(number instanceof Byte) {
                return derived(-number.byteValue());
            } else if(number instanceof Short) {
                return derived(-number.shortValue());
            } else if(number instanceof Integer) {
                return derived(-number.intValue());
            } else if(number instanceof Long) {
                return derived(-number.longValue());
            } else if(number instanceof BigInteger) {
                return derived(((BigInteger) number).negate());
            } else if(number instanceof Float) {
                return derived(-number.floatValue());
            } else if(number instanceof Double) {
                return derived(-d);
            } else if(number instanceof BigDecimal) {
                return derived(((BigDecimal) number).negate());
            } else if(number instanceof BigFraction) {
                return derived(((BigFraction) number).negate());
            } else {
                return new FailWithMessage(name + " not implemented for number type: " + number.getClass().getSimpleName());
            }
        } else if(Operator.ABSVALUE.getMethodName().equals(name)) {
            if(number instanceof Byte) {
                return derived((byte) Math.abs(number.byteValue()));
            } else if(number instanceof Short) {
                return derived((short) Math.abs(number.shortValue()));
            } else if(number instanceof Integer) {
                return derived(Math.abs(number.intValue()));
            } else if(number instanceof Long) {
                return derived(Math.abs(number.longValue()));
            } else if(number instanceof BigInteger) {
                return derived(((BigInteger) number).abs());
            } else if(number instanceof Float) {
                return derived(Math.abs(number.floatValue()));
            } else if(number instanceof Double) {
                return derived(Math.abs(d));
            } else if(number instanceof BigDecimal) {
                return derived(((BigDecimal) number).abs());
            } else if(number instanceof BigFraction) {
                return derived(((BigFraction) number).abs());
            } else {
                return new FailWithMessage(name + " not implemented for number type: " + number.getClass().getSimpleName());
            }
        } else if(Operator.PLUS.getMethodName().equals(name)) {
            return this;
        } else if("doubled".equals(name)) {
            if(number instanceof Byte) {
                return derived(number.byteValue() << 1);
            } else if(number instanceof Short) {
                return derived(number.shortValue() << 1);
            } else if(number instanceof Integer) {
                return derived(number.intValue() << 1);
            } else if(number instanceof Long) {
                return derived(number.longValue() << 1);
            } else if(number instanceof BigInteger) {
                return derived(((BigInteger) number).add((BigInteger) number));
            } else if(number instanceof Float) {
                return derived(number.floatValue() * 2);
            } else if(number instanceof Double) {
                return derived(d * 2);
            } else if(number instanceof BigDecimal) {
                return derived(((BigDecimal) number).add((BigDecimal) number));
            } else if(number instanceof BigFraction) {
                return derived(((BigFraction) number).add((BigFraction) number));
            } else {
                return new FailWithMessage(name + " not implemented for number type: " + number.getClass().getSimpleName());
            }
        } else if("bsr1".equals(name)) {
            if(number instanceof Byte) {
                return derived(number.byteValue() >> 1);
            } else if(number instanceof Short) {
                return derived(number.shortValue() >> 1);
            } else if(number instanceof Integer) {
                return derived(number.intValue() >> 1);
            } else if(number instanceof Long) {
                return derived(number.longValue() >> 1);
            } else if(number instanceof BigInteger) {
                return derived(((BigInteger) number).shiftRight(1));
            } else if(number instanceof Float) {
                return derived(Math.floor(number.floatValue() / 2));
            } else if(number instanceof Double) {
                return derived(Math.floor(number.doubleValue() / 2));
            } else if(number instanceof BigDecimal) {
                return derived(((BigDecimal) number).divide(BigDecimal.valueOf(2)).setScale(0, RoundingMode.FLOOR));
            } else if(number instanceof BigFraction) {
                BigFraction bf = (BigFraction) number;
                return derived(bf.getNumerator().divide(bf.getDenominator()).shiftRight(1));
            } else {
                return new FailWithMessage(name + " not implemented for number type: " + number.getClass().getSimpleName());
            }
        } else if("is even".equals(name)) {
            if(number instanceof Byte) {
                return bool((number.byteValue() & 1) == 0);
            } else if(number instanceof Short) {
                return bool((number.shortValue() & 1) == 0);
            } else if(number instanceof Integer) {
                return bool((number.intValue() & 1) == 0);
            } else if(number instanceof Long) {
                return bool((number.longValue() & 1) == 0);
            } else if(number instanceof BigInteger) {
                return bool(((BigInteger) number).testBit(0) == false);
            } else if(number instanceof Double || number instanceof Float) {
                return bool(Math.rint(d) == d && Double.isFinite(d) && (((long) d) & 1) == 0);
            } else if(number instanceof BigDecimal) {
                BigDecimal bd = (BigDecimal) number;
                boolean isInteger = bd.signum() == 0 || bd.scale() <= 0 || bd.stripTrailingZeros().scale() <= 0;
                return bool(isInteger && bd.toBigInteger().testBit(0) == false);
            } else if(number instanceof BigFraction) {
                return bool(((BigFraction) number).getDenominator().equals(BigInteger.ONE) && ((BigFraction) number).getNumerator().testBit(0) == false);
            } else {
                return new FailWithMessage(name + " not implemented for number type: " + number.getClass().getSimpleName());
            }
        } else if("is odd".equals(name)) {
            if(number instanceof Byte) {
                return bool((number.byteValue() & 1) == 1);
            } else if(number instanceof Short) {
                return bool((number.shortValue() & 1) == 1);
            } else if(number instanceof Integer) {
                return bool((number.intValue() & 1) == 1);
            } else if(number instanceof Long) {
                return bool((number.longValue() & 1) == 1);
            } else if(number instanceof BigInteger) {
                return bool(((BigInteger) number).testBit(0) == true);
            } else if(number instanceof Double || number instanceof Float) {
                return bool(Math.rint(d) == d && Double.isFinite(d) && (((long) d) & 1) == 1);
            } else if(number instanceof BigDecimal) {
                BigDecimal bd = (BigDecimal) number;
                boolean isInteger = bd.signum() == 0 || bd.scale() <= 0 || bd.stripTrailingZeros().scale() <= 0;                
                return bool(isInteger && bd.toBigInteger().testBit(0) == true);
            } else if(number instanceof BigFraction) {
                return bool(((BigFraction) number).getDenominator().equals(BigInteger.ONE) && ((BigFraction) number).getNumerator().testBit(0) == true);
            } else {
                return new FailWithMessage(name + " not implemented for number type: " + number.getClass().getSimpleName());
            }
        } else if("is zero".equals(name)) {
            if(number instanceof Byte) {
                return bool(number.byteValue() == 0);
            } else if(number instanceof Short) {
                return bool(number.shortValue() == 0);
            } else if(number instanceof Integer) {
                return bool(number.intValue() == 0);
            } else if(number instanceof Long) {
                return bool(number.longValue() == 0);
            } else if(number instanceof BigInteger) {
                return bool(((BigInteger) number).signum() == 0);
            } else if(number instanceof Float) {
                return bool(number.floatValue() == 0.0f);
            } else if(number instanceof Double) {
                return bool(number.doubleValue() == 0.0);
            } else if(number instanceof BigDecimal) {
                return bool(((BigDecimal) number).signum() == 0);
            } else if(number instanceof BigFraction) {
                return bool(((BigFraction) number).equals(BigFraction.ZERO));
            } else {
                return new FailWithMessage(name + " not implemented for number type: " + number.getClass().getSimpleName());
            }
        } else if("is positive".equals(name)) {
            if(number instanceof Byte) {
                return bool(number.byteValue() > 0);
            } else if(number instanceof Short) {
                return bool(number.shortValue() > 0);
            } else if(number instanceof Integer) {
                return bool(number.intValue() > 0);
            } else if(number instanceof Long) {
                return bool(number.longValue() > 0);
            } else if(number instanceof BigInteger) {
                return bool(((BigInteger) number).signum() == 1);
            } else if(number instanceof Float) {
                return bool(Math.signum(number.floatValue()) == 1.0f);
            } else if(number instanceof Double) {
                return bool(Math.signum(number.doubleValue()) == 1.0);
            } else if(number instanceof BigDecimal) {
                return bool(((BigDecimal) number).signum() == 1);
            } else if(number instanceof BigFraction) {
                return bool(((BigFraction) number).compareTo(BigFraction.ZERO) == 1);
            } else {
                return new FailWithMessage(name + " not implemented for number type: " + number.getClass().getSimpleName());
            }
        } else if("is negative".equals(name)) {
            if(number instanceof Byte) {
                return bool(number.byteValue() < 0);
            } else if(number instanceof Short) {
                return bool(number.shortValue() < 0);
            } else if(number instanceof Integer) {
                return bool(number.intValue() < 0);
            } else if(number instanceof Long) {
                return bool(number.longValue() < 0);
            } else if(number instanceof BigInteger) {
                return bool(((BigInteger) number).signum() == -1);
            } else if(number instanceof Float) {
                return bool(Math.signum(number.floatValue()) == -1.0f);
            } else if(number instanceof Double) {
                return bool(Math.signum(number.doubleValue()) == -1.0);
            } else if(number instanceof BigDecimal) {
                return bool(((BigDecimal) number).signum() == -1);
            } else if(number instanceof BigFraction) {
                return bool(((BigFraction) number).compareTo(BigFraction.ZERO) == -1);
            } else {
                return new FailWithMessage(name + " not implemented for number type: " + number.getClass().getSimpleName());
            }
        } else if("label".equals(name)) {
            return new KernelStringValue(number.toString(), trueValue, falseValue);
        }

        return Value.super.slot(self, name, ranges, fallback);

    }

    @Override
    public <T> Either<T, Fail> convertToJava(Class<T> clazz) {
        if(clazz.isInstance(number))
            return Either.left(clazz.cast(number));
        return Value.super.convertToJava(clazz);
    }
}
