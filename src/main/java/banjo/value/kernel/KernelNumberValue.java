package banjo.value.kernel;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.util.function.Function;

import org.apache.commons.math3.fraction.BigFraction;

import banjo.expr.source.Operator;
import banjo.expr.util.SourceFileRange;
import banjo.value.BaseValueVisitor;
import banjo.value.Value;
import banjo.value.ValueVisitor;
import banjo.value.fail.FailWithMessage;
import fj.F;
import fj.F0;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;

public class KernelNumberValue extends KernelValueWrapper<Number> implements Value {

    public KernelNumberValue(Number number, Value trueValue) {
        super(number, trueValue);
    }

    /**
     * Return a new KernelNumberValue with a (probably) different number but the
     * same reference to true and false that we need for the boolean slot
     * implementations.
     */
    private Value derived(Number n) {
        return new KernelNumberValue(n, trueValue);
    }

    /**
     * Extract a Number instance from the given value.
     * @param trace TODO
     */
    private static Option<Number> extractNumber(List<Value> trace, Value v) {
        return v.slot(trace, "kernel number value").acceptVisitor(new BaseValueVisitor<Option<Number>>() {
            @Override
            public Option<Number> fallback() {
                return Option.none();
            }

            @Override
            public Option<Number> kernelNumber(KernelNumberValue kernelNumberValue) {
                return Option.some(kernelNumberValue.value);
            }
        });
    }

    /**
     * Convert some Number value to BigInteger.
     */
    public static BigInteger bigIntegerValue(Number number) {
        if(number instanceof BigInteger) {
            return (BigInteger) number;
        } else {
            return BigInteger.valueOf(number.longValue());
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

    public static interface NumberBinaryOp<T> {
        T int8(byte a, byte b);

        T int16(short a, short b);

        T int32(int a, int b);

        T int64(long a, long b);

        T integer(BigInteger a, BigInteger b);

        T float32(float a, float b);

        T float64(double a, double b);

        T decimal(BigDecimal a, BigDecimal b);

        T fraction(BigFraction a, BigFraction b);

        public default <U> NumberBinaryOp<U> andThen(Function<T, U> f) {
            NumberBinaryOp<T> impl = this;
            return new NumberBinaryOp<U>() {
                @Override
                public U int8(byte a, byte b) {
                    return f.apply(impl.int8(a, b));
                }

                @Override
                public U int16(short a, short b) {
                    return f.apply(impl.int16(a, b));
                }

                @Override
                public U int32(int a, int b) {
                    return f.apply(impl.int32(a, b));
                }

                @Override
                public U int64(long a, long b) {
                    return f.apply(impl.int64(a, b));
                }

                @Override
                public U integer(BigInteger a, BigInteger b) {
                    return f.apply(impl.integer(a, b));
                }

                @Override
                public U float32(float a, float b) {
                    return f.apply(impl.float32(a, b));
                }

                @Override
                public U float64(double a, double b) {
                    return f.apply(impl.float64(a, b));
                }

                @Override
                public U decimal(BigDecimal a, BigDecimal b) {
                    return f.apply(impl.decimal(a, b));
                }

                @Override
                public U fraction(BigFraction a, BigFraction b) {
                    return f.apply(impl.fraction(a, b));
                }
            };
        }
    }

    public static NumberBinaryOp<Number> ADD_OP = new NumberBinaryOp<Number>() {

        @Override
        public Number int8(byte a, byte b) {
            return (byte) (a + b);
        }

        @Override
        public Number int16(short a, short b) {
            return (short) (a + b);
        }

        @Override
        public Number int32(int a, int b) {
            return a + b;
        }

        @Override
        public Number int64(long a, long b) {
            return a + b;
        }

        @Override
        public Number integer(BigInteger a, BigInteger b) {
            return a.add(b);
        }

        @Override
        public Number float32(float a, float b) {
            return a + b;
        }

        @Override
        public Number float64(double a, double b) {
            return a + b;
        }

        @Override
        public Number decimal(BigDecimal a, BigDecimal b) {
            return a.add(b);
        }

        @Override
        public Number fraction(BigFraction a, BigFraction b) {
            return a.add(b);
        }

    };

    public static NumberBinaryOp<Number> SUB_OP = new NumberBinaryOp<Number>() {

        @Override
        public Number int8(byte a, byte b) {
            return (byte) (a - b);
        }

        @Override
        public Number int16(short a, short b) {
            return (short) (a - b);
        }

        @Override
        public Number int32(int a, int b) {
            return a - b;
        }

        @Override
        public Number int64(long a, long b) {
            return a - b;
        }

        @Override
        public Number integer(BigInteger a, BigInteger b) {
            return a.subtract(b);
        }

        @Override
        public Number float32(float a, float b) {
            return a - b;
        }

        @Override
        public Number float64(double a, double b) {
            return a - b;
        }

        @Override
        public Number decimal(BigDecimal a, BigDecimal b) {
            return a.subtract(b);
        }

        @Override
        public Number fraction(BigFraction a, BigFraction b) {
            return a.subtract(b);
        }

    };

    public static NumberBinaryOp<Number> MUL_OP = new NumberBinaryOp<Number>() {

        @Override
        public Number int8(byte a, byte b) {
            return (byte) (a * b);
        }

        @Override
        public Number int16(short a, short b) {
            return (short) (a * b);
        }

        @Override
        public Number int32(int a, int b) {
            return a * b;
        }

        @Override
        public Number int64(long a, long b) {
            return a * b;
        }

        @Override
        public Number integer(BigInteger a, BigInteger b) {
            return a.multiply(b);
        }

        @Override
        public Number float32(float a, float b) {
            return a * b;
        }

        @Override
        public Number float64(double a, double b) {
            return a * b;
        }

        @Override
        public Number decimal(BigDecimal a, BigDecimal b) {
            return a.multiply(b);
        }

        @Override
        public Number fraction(BigFraction a, BigFraction b) {
            return a.multiply(b);
        }

    };

    public static NumberBinaryOp<Number> DIV_OP = new NumberBinaryOp<Number>() {

        @Override
        public Number int8(byte a, byte b) {
            return (byte) (a / b);
        }

        @Override
        public Number int16(short a, short b) {
            return (short) (a / b);
        }

        @Override
        public Number int32(int a, int b) {
            return a / b;
        }

        @Override
        public Number int64(long a, long b) {
            return a / b;
        }

        @Override
        public Number integer(BigInteger a, BigInteger b) {
            return a.divide(b);
        }

        @Override
        public Number float32(float a, float b) {
            return a / b;
        }

        @Override
        public Number float64(double a, double b) {
            return a / b;
        }

        @Override
        public Number decimal(BigDecimal a, BigDecimal b) {
            return a.divide(b);
        }

        @Override
        public Number fraction(BigFraction a, BigFraction b) {
            return a.divide(b);
        }

    };

    public static NumberBinaryOp<Number> BSL_OP = new NumberBinaryOp<Number>() {

        @Override
        public Number int8(byte a, byte b) {
            return (byte) (a << b);
        }

        @Override
        public Number int16(short a, short b) {
            return (short) (a << b);
        }

        @Override
        public Number int32(int a, int b) {
            return a << b;
        }

        @Override
        public Number int64(long a, long b) {
            return a << b;
        }

        @Override
        public Number integer(BigInteger a, BigInteger b) {
            return a.shiftLeft(b.intValue());
        }

        @Override
        public Number float32(float a, float b) {
            return (int)a << (int)b;
        }

        @Override
        public Number float64(double a, double b) {
            return (long)a << (int)b;
        }

        @Override
        public Number decimal(BigDecimal a, BigDecimal b) {
            return integer(a.toBigInteger(), b.toBigInteger());
        }

        @Override
        public Number fraction(BigFraction a, BigFraction b) {
            return integer(a.getNumerator().divide(a.getDenominator()), b.getNumerator().divide(b.getDenominator()));
        }

    };

    public static NumberBinaryOp<Boolean> EQ_OP = new NumberBinaryOp<Boolean>() {

        @Override
        public Boolean int8(byte a, byte b) {
            return a == b;
        }

        @Override
        public Boolean int16(short a, short b) {
            return a == b;
        }

        @Override
        public Boolean int32(int a, int b) {
            return a == b;
        }

        @Override
        public Boolean int64(long a, long b) {
            return a == b;
        }

        @Override
        public Boolean integer(BigInteger a, BigInteger b) {
            return a.compareTo(b) == 0;
        }

        @Override
        public Boolean float32(float a, float b) {
            return a == b;
        }

        @Override
        public Boolean float64(double a, double b) {
            return a == b;
        }

        @Override
        public Boolean decimal(BigDecimal a, BigDecimal b) {
            return a.compareTo(b) == 0;
        }

        @Override
        public Boolean fraction(BigFraction a, BigFraction b) {
            return a.compareTo(b) == 0;
        }

    };

    public static NumberBinaryOp<Boolean> LT_OP = new NumberBinaryOp<Boolean>() {

        @Override
        public Boolean int8(byte a, byte b) {
            return a < b;
        }

        @Override
        public Boolean int16(short a, short b) {
            return a < b;
        }

        @Override
        public Boolean int32(int a, int b) {
            return a < b;
        }

        @Override
        public Boolean int64(long a, long b) {
            return a < b;
        }

        @Override
        public Boolean integer(BigInteger a, BigInteger b) {
            return a.compareTo(b) < 0;
        }

        @Override
        public Boolean float32(float a, float b) {
            return a < b;
        }

        @Override
        public Boolean float64(double a, double b) {
            return a < b;
        }

        @Override
        public Boolean decimal(BigDecimal a, BigDecimal b) {
            return a.compareTo(b) < 0;
        }

        @Override
        public Boolean fraction(BigFraction a, BigFraction b) {
            return a.compareTo(b) < 0;
        }

    };

    public Value callBinaryOp(List<Value> trace, NumberBinaryOp<Value> op, Value operand, String operation) {
        F<Number, Value> f =
            n -> 
            (value instanceof Byte) ? op.int8(value.byteValue(), n.byteValue()) :
            (value instanceof Short) ? op.int16(value.shortValue(), n.shortValue()) :
            (value instanceof Integer) ? op.int32(value.intValue(), n.intValue()) :
            (value instanceof Long) ? op.int64(value.longValue(), n.longValue()) :
            (value instanceof BigInteger) ? op.integer((BigInteger) value, bigIntegerValue(n)) :
            (value instanceof Float) ? op.float32(value.floatValue(), n.floatValue()) :
            (value instanceof Double) ? op.float64(value.doubleValue(), n.doubleValue()) :
            (value instanceof BigDecimal) ? op.decimal((BigDecimal) value, bigDecimalValue(n)) :
            (value instanceof BigFraction) ? op.fraction((BigFraction) value, bigFractionValue(n)) :
 (Value) new FailWithMessage(trace,
                                                    operation + " not implemented for number type: " + value.getClass().getSimpleName());
        F0<Value> fail = () -> new FailWithMessage(trace, "Can only " + operation + " kernel numbers with kernel numbers");
        return extractNumber(trace, operand).map(f).orSome(fail);
    }

    public Value binaryOp(List<Value> trace, NumberBinaryOp<Value> op, String operation) {
        return Value.function((Value operand) -> callBinaryOp(trace, op, operand, operation));
    }

    public Value booleanBinaryOp(List<Value> trace, NumberBinaryOp<Boolean> op, String operation) {
        return binaryOp(trace, op.andThen(this::boolValue), operation);
    }

    public Value numberBinaryOp(List<Value> trace, NumberBinaryOp<Number> op, String operation) {
        return binaryOp(trace, op.andThen(this::derived), operation);
    }

    /**
     * Try and calculate a slot of this number
     */
    @Override
    public Value slot(List<Value> trace, Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
        // Retrieve raw kernel number
        if("kernel number value".equals(name)) {
            return this;
        }
        
        // Type checks
        if("is int8".equals(name)) {
            return valueIsInstanceOf(Byte.class);
        } else if("is int16".equals(name)) {
            return valueIsInstanceOf(Short.class);
        } else if("is int32".equals(name)) {
            return valueIsInstanceOf(Integer.class);
        } else if("is int64".equals(name)) {
            return valueIsInstanceOf(Long.class);
        } else if("is integer".equals(name)) {
            return valueIsInstanceOf(BigInteger.class);
        } else if("is float32".equals(name)) {
            return valueIsInstanceOf(Float.class);            
        } else if("is float64".equals(name)) {
            return valueIsInstanceOf(Double.class);
        } else if("is decimal".equals(name)) {
            return valueIsInstanceOf(BigDecimal.class);
        } else if("is fraction".equals(name)) {
            return valueIsInstanceOf(BigFraction.class);
        }

        // Conversions
        double d = value.doubleValue();
        if("int8".equals(name)) {
            if(value instanceof Byte) {
                return this;
            } else {
                return derived(value.byteValue());
            }
        } else if("int16".equals(name)) {
            if(value instanceof Short) {
                return this;
            } else {
                return derived(value.shortValue());
            }
        } else if("int32".equals(name)) {
            if(value instanceof Integer) {
                return this;
            } else {
                return derived(value.intValue());
            }
        } else if("int64".equals(name)) {
            if(value instanceof Long) {
                return this;
            } else {
                return derived(value.longValue());
            }
        } else if("integer".equals(name)) {
            if(value instanceof BigInteger) {
                return this;
            } else {
                return derived(bigIntegerValue(value));
            }
        } else if("float32".equals(name)) {
            if(value instanceof Float) {
                return this;
            } else {
                return derived(value.floatValue());
            }
        } else if("float64".equals(name)) {
            if(value instanceof Double) {
                return this;
            } else {
                return derived(d);
            }
        } else if("decimal".equals(name)) {
            if(value instanceof BigDecimal) {
                return this;
            } else {
                return derived(bigDecimalValue(value));
            }
            
        } else if("fraction".equals(name)) {
            if(value instanceof BigFraction) {
                return this;
            } else {
                return derived(bigFractionValue(value));
            }
        }

        // Binary operators
        if(Operator.ADD.getMethodName().equals(name)) {
            return numberBinaryOp(trace, ADD_OP, "add");
        } else if(Operator.SUB.getMethodName().equals(name)) {
            return numberBinaryOp(trace, SUB_OP, "subtract");
        } else if(Operator.MUL.getMethodName().equals(name)) {
            return numberBinaryOp(trace, MUL_OP, "multiply");
        } else if(Operator.DIV.getMethodName().equals(name)) {
            return numberBinaryOp(trace, DIV_OP, "divide");
        } else if("bit shifted".equals(name)) {
            return numberBinaryOp(trace, BSL_OP, "bit shift left");
        } else if(Operator.EQ.getMethodName().equals(name)) {
            return booleanBinaryOp(trace, EQ_OP, "compare");
        } else if(Operator.LT.getMethodName().equals(name)) {
            return booleanBinaryOp(trace, LT_OP, "compare");
        }

        // Unary operators
        if(Operator.NEGATE.getMethodName().equals(name)) {
            if(value instanceof Byte) {
                return derived(-value.byteValue());
            } else if(value instanceof Short) {
                return derived(-value.shortValue());
            } else if(value instanceof Integer) {
                return derived(-value.intValue());
            } else if(value instanceof Long) {
                return derived(-value.longValue());
            } else if(value instanceof BigInteger) {
                return derived(((BigInteger) value).negate());
            } else if(value instanceof Float) {
                return derived(-value.floatValue());
            } else if(value instanceof Double) {
                return derived(-d);
            } else if(value instanceof BigDecimal) {
                return derived(((BigDecimal) value).negate());
            } else if(value instanceof BigFraction) {
                return derived(((BigFraction) value).negate());
            } else {
                return new FailWithMessage(trace, name + " not implemented for number type: " + value.getClass().getSimpleName());
            }
        } else if(Operator.ABSVALUE.getMethodName().equals(name)) {
            if(value instanceof Byte) {
                return derived((byte) Math.abs(value.byteValue()));
            } else if(value instanceof Short) {
                return derived((short) Math.abs(value.shortValue()));
            } else if(value instanceof Integer) {
                return derived(Math.abs(value.intValue()));
            } else if(value instanceof Long) {
                return derived(Math.abs(value.longValue()));
            } else if(value instanceof BigInteger) {
                return derived(((BigInteger) value).abs());
            } else if(value instanceof Float) {
                return derived(Math.abs(value.floatValue()));
            } else if(value instanceof Double) {
                return derived(Math.abs(d));
            } else if(value instanceof BigDecimal) {
                return derived(((BigDecimal) value).abs());
            } else if(value instanceof BigFraction) {
                return derived(((BigFraction) value).abs());
            } else {
                return new FailWithMessage(trace, name + " not implemented for number type: " + value.getClass().getSimpleName());
            }
        } else if(Operator.PLUS.getMethodName().equals(name)) {
            return this;
        } else if("doubled".equals(name)) {
            if(value instanceof Byte) {
                return derived(value.byteValue() << 1);
            } else if(value instanceof Short) {
                return derived(value.shortValue() << 1);
            } else if(value instanceof Integer) {
                return derived(value.intValue() << 1);
            } else if(value instanceof Long) {
                return derived(value.longValue() << 1);
            } else if(value instanceof BigInteger) {
                return derived(((BigInteger) value).add((BigInteger) value));
            } else if(value instanceof Float) {
                return derived(value.floatValue() * 2);
            } else if(value instanceof Double) {
                return derived(d * 2);
            } else if(value instanceof BigDecimal) {
                return derived(((BigDecimal) value).add((BigDecimal) value));
            } else if(value instanceof BigFraction) {
                return derived(((BigFraction) value).add((BigFraction) value));
            } else {
                return new FailWithMessage(trace, name + " not implemented for number type: " + value.getClass().getSimpleName());
            }
        } else if("bsr1".equals(name)) {
            if(value instanceof Byte) {
                return derived(value.byteValue() >> 1);
            } else if(value instanceof Short) {
                return derived(value.shortValue() >> 1);
            } else if(value instanceof Integer) {
                return derived(value.intValue() >> 1);
            } else if(value instanceof Long) {
                return derived(value.longValue() >> 1);
            } else if(value instanceof BigInteger) {
                return derived(((BigInteger) value).shiftRight(1));
            } else if(value instanceof Float) {
                return derived(Math.floor(value.floatValue() / 2));
            } else if(value instanceof Double) {
                return derived(Math.floor(value.doubleValue() / 2));
            } else if(value instanceof BigDecimal) {
                return derived(((BigDecimal) value).divide(BigDecimal.valueOf(2)).setScale(0, RoundingMode.FLOOR));
            } else if(value instanceof BigFraction) {
                BigFraction bf = (BigFraction) value;
                return derived(bf.getNumerator().divide(bf.getDenominator()).shiftRight(1));
            } else {
                return new FailWithMessage(trace, name + " not implemented for number type: " + value.getClass().getSimpleName());
            }
        } else if("is even".equals(name)) {
            if(value instanceof Byte) {
                return boolValue((value.byteValue() & 1) == 0);
            } else if(value instanceof Short) {
                return boolValue((value.shortValue() & 1) == 0);
            } else if(value instanceof Integer) {
                return boolValue((value.intValue() & 1) == 0);
            } else if(value instanceof Long) {
                return boolValue((value.longValue() & 1) == 0);
            } else if(value instanceof BigInteger) {
                return boolValue(((BigInteger) value).testBit(0) == false);
            } else if(value instanceof Double || value instanceof Float) {
                return boolValue(Math.rint(d) == d && Double.isFinite(d) && (((long) d) & 1) == 0);
            } else if(value instanceof BigDecimal) {
                BigDecimal bd = (BigDecimal) value;
                boolean isInteger = bd.signum() == 0 || bd.scale() <= 0 || bd.stripTrailingZeros().scale() <= 0;
                return boolValue(isInteger && bd.toBigInteger().testBit(0) == false);
            } else if(value instanceof BigFraction) {
                return boolValue(((BigFraction) value).getDenominator().equals(BigInteger.ONE) && ((BigFraction) value).getNumerator().testBit(0) == false);
            } else {
                return new FailWithMessage(trace, name + " not implemented for number type: " + value.getClass().getSimpleName());
            }
        } else if("is odd".equals(name)) {
            if(value instanceof Byte) {
                return boolValue((value.byteValue() & 1) == 1);
            } else if(value instanceof Short) {
                return boolValue((value.shortValue() & 1) == 1);
            } else if(value instanceof Integer) {
                return boolValue((value.intValue() & 1) == 1);
            } else if(value instanceof Long) {
                return boolValue((value.longValue() & 1) == 1);
            } else if(value instanceof BigInteger) {
                return boolValue(((BigInteger) value).testBit(0) == true);
            } else if(value instanceof Double || value instanceof Float) {
                return boolValue(Math.rint(d) == d && Double.isFinite(d) && (((long) d) & 1) == 1);
            } else if(value instanceof BigDecimal) {
                BigDecimal bd = (BigDecimal) value;
                boolean isInteger = bd.signum() == 0 || bd.scale() <= 0 || bd.stripTrailingZeros().scale() <= 0;                
                return boolValue(isInteger && bd.toBigInteger().testBit(0) == true);
            } else if(value instanceof BigFraction) {
                return boolValue(((BigFraction) value).getDenominator().equals(BigInteger.ONE) && ((BigFraction) value).getNumerator().testBit(0) == true);
            } else {
                return new FailWithMessage(trace, name + " not implemented for number type: " + value.getClass().getSimpleName());
            }
        } else if("is zero".equals(name)) {
            if(value instanceof Byte) {
                return boolValue(value.byteValue() == 0);
            } else if(value instanceof Short) {
                return boolValue(value.shortValue() == 0);
            } else if(value instanceof Integer) {
                return boolValue(value.intValue() == 0);
            } else if(value instanceof Long) {
                return boolValue(value.longValue() == 0);
            } else if(value instanceof BigInteger) {
                return boolValue(((BigInteger) value).signum() == 0);
            } else if(value instanceof Float) {
                return boolValue(value.floatValue() == 0.0f);
            } else if(value instanceof Double) {
                return boolValue(value.doubleValue() == 0.0);
            } else if(value instanceof BigDecimal) {
                return boolValue(((BigDecimal) value).signum() == 0);
            } else if(value instanceof BigFraction) {
                return boolValue(((BigFraction) value).equals(BigFraction.ZERO));
            } else {
                return new FailWithMessage(trace, name + " not implemented for number type: " + value.getClass().getSimpleName());
            }
        } else if("is positive".equals(name)) {
            if(value instanceof Byte) {
                return boolValue(value.byteValue() > 0);
            } else if(value instanceof Short) {
                return boolValue(value.shortValue() > 0);
            } else if(value instanceof Integer) {
                return boolValue(value.intValue() > 0);
            } else if(value instanceof Long) {
                return boolValue(value.longValue() > 0);
            } else if(value instanceof BigInteger) {
                return boolValue(((BigInteger) value).signum() == 1);
            } else if(value instanceof Float) {
                return boolValue(Math.signum(value.floatValue()) == 1.0f);
            } else if(value instanceof Double) {
                return boolValue(Math.signum(value.doubleValue()) == 1.0);
            } else if(value instanceof BigDecimal) {
                return boolValue(((BigDecimal) value).signum() == 1);
            } else if(value instanceof BigFraction) {
                return boolValue(((BigFraction) value).compareTo(BigFraction.ZERO) == 1);
            } else {
                return new FailWithMessage(trace, name + " not implemented for number type: " + value.getClass().getSimpleName());
            }
        } else if("is negative".equals(name)) {
            if(value instanceof Byte) {
                return boolValue(value.byteValue() < 0);
            } else if(value instanceof Short) {
                return boolValue(value.shortValue() < 0);
            } else if(value instanceof Integer) {
                return boolValue(value.intValue() < 0);
            } else if(value instanceof Long) {
                return boolValue(value.longValue() < 0);
            } else if(value instanceof BigInteger) {
                return boolValue(((BigInteger) value).signum() == -1);
            } else if(value instanceof Float) {
                return boolValue(Math.signum(value.floatValue()) == -1.0f);
            } else if(value instanceof Double) {
                return boolValue(Math.signum(value.doubleValue()) == -1.0);
            } else if(value instanceof BigDecimal) {
                return boolValue(((BigDecimal) value).signum() == -1);
            } else if(value instanceof BigFraction) {
                return boolValue(((BigFraction) value).compareTo(BigFraction.ZERO) == -1);
            } else {
                return new FailWithMessage(trace, name + " not implemented for number type: " + value.getClass().getSimpleName());
            }
        } else if("label".equals(name)) {
            return new KernelStringValue(value.toString(), trueValue);
        }

        return Value.super.slot(trace, self, name, ranges, fallback);

    }

    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.kernelNumber(this);
    }
}
