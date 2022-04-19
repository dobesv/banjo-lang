package banjo.expr.token;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Objects;

import org.apache.commons.math3.fraction.BigFraction;

import banjo.expr.BadExpr;
import banjo.expr.core.BaseCoreExprVisitor;
import banjo.expr.core.CoreExpr;
import banjo.expr.core.CoreExprAlgebra;
import banjo.expr.core.CoreExprVisitor;
import banjo.expr.source.Precedence;
import banjo.expr.source.SourceExprAlgebra;
import banjo.expr.source.SourceExprVisitor;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.data.List;
import fj.data.Set;


public class NumberLiteral extends AbstractAtom implements Atom {
    public static final Ord<NumberLiteral> ORD = Ord.stringOrd.contramap((NumberLiteral n) -> n.source);
    public final Number number;
    public final String source;

    public NumberLiteral(Set<SourceFileRange> ranges, int indentColumn, Number number, String source) {
        super(ranges, indentColumn);
        this.number = number;
        this.source = source;
	}

    public NumberLiteral(SourceFileRange sfr, int indentColumn, Number number, String source) {
        this(Set.single(SourceFileRange.ORD, sfr), indentColumn, number, source);
    }

    public NumberLiteral(Set<SourceFileRange> ranges, Number number, String source) {
        this(ranges, 0, number, source);
    }

    public NumberLiteral(Number n, String source) {
        this(SourceFileRange.EMPTY_SET, 0, n, source);
	}

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
        return visitor.numberLiteral(this);
	}

    public static boolean isNumberLiteral(CoreExpr x) {
		return Objects.requireNonNull(x.acceptVisitor(new BaseCoreExprVisitor<Boolean>() {
			@Override
            public Boolean numberLiteral(NumberLiteral n) {
				return true;
			}
			@Override
			public Boolean fallback() {
				return false;
			}
		})).booleanValue();
	}

	@Override
	public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
        return visitor.numberLiteral(getRanges(), number, source);
	}

	@Override
	public <T> T acceptVisitor(SourceExprAlgebra<T> visitor) {
		return visitor.numberLiteral(getRanges(), number);
	}

	@Override
	public <T> T acceptVisitor(TokenVisitor<T> parser) {
        return parser.numberLiteral(getRanges().toStream().head().getFileRange(), indentColumn, number, source);
	}

    public NumberLiteral negate(Set<SourceFileRange> ranges) {
        return new NumberLiteral(ranges, indentColumn, ExactArithmeticOperators.INSTANCE.negate(number),
                source.charAt(0) == '-' ? source.substring(1) : "-" + source);
    }

    public CoreExpr absValue(Set<SourceFileRange> ranges) {
        return ExactArithmeticOperators.INSTANCE.isNegative(number) ? negate(ranges) : this;
    }

    public int signum() {
        return ExactArithmeticOperators.INSTANCE.signum(this.number);
    }

    /**
     * Given a Number but no source for it, generate the appropriate source token for
     * the number and create a KernelNumberLiteral instance.
     */
    public static NumberLiteral fromNumber(Number number) {
        return new NumberLiteral(number, number.toString());
    }

    @Override
    public String toSource() {
        return source;
    }

    public static interface ArithmeticOperators<T extends Number> {
        public T convert(Number a);

        public T convertExact(Number a) throws ArithmeticException;

        public default Number sumConverted(Number a, Number b) {
            return sum(convert(a), convert(b));
        }

        public T sum(T a, T b);

        public T sumExact(T a, T b) throws ArithmeticException;

        public default Number differenceConverted(Number a, Number b) {
            return difference(convert(a), convert(b));
        }

        public T difference(T a, T b);

        public T differenceExact(T a, T b) throws ArithmeticException;

        public default Number productConverted(Number a, Number b) {
            return product(convert(a), convert(b));
        }

        public T product(T a, T b);

        public T productExact(T a, T b) throws ArithmeticException;

        public default Number quotientConverted(Number a, Number b) {
            return quotient(convert(a), convert(b));
        }

        public T quotient(T a, T b);

        public T quotientExact(T a, T b) throws ArithmeticException;

        public default Number remainderConverted(Number divisor, Number dividend) {
            return remainder(convert(divisor), convert(dividend));
        }

        public T remainder(T divisor, T dividend);

        public default boolean equalsConverted(Number a, Number b) {
            return equals(convert(a), convert(b));
        }

        public default boolean equals(T a, T b) {
            return a.equals(b);
        }

        public default boolean zeroConverted(Number a) {
            return isZero(convert(a));
        }

        public boolean isZero(T a);

        public default int cmpConverted(Number a, Number b) {
            return cmp(convert(a), convert(b));
        }

        public int cmp(T a, T b);

        public default Number negateConverted(Number a) {
            return negate(convert(a));
        }

        public T negate(T a);

        public default int signumConverted(Number a) {
            return signum(convert(a));
        }

        public default int signum(T a) {
            return cmp(a, negate(a));
        }

        public default Number absConverted(Number a) {
            return abs(convert(a));
        }

        public default T abs(T a) {
            return isNegative(a) ? negate(a) : a;
        }

        public default boolean isNegative(T a) {
            return signum(a) == -1;
        }

        public default boolean isPositive(T a) {
            return signum(a) == 1;
        }

        public T reciprocal(T a);

        public default Number reciprocalConverted(Number a) {
            return reciprocal(convert(a));
        }

        public default boolean ascending(T a, T b) {
            return cmp(a, b) < 0;
        }

        public default boolean ascendingConverted(Number a, Number b) {
            return ascending(convert(a), convert(b));
        }

        public default boolean descending(T a, T b) {
            return cmp(a, b) > 0;
        }

        public default boolean descendingConverted(Number a, Number b) {
            return descending(convert(a), convert(b));
        }

        public default boolean isOddConverted(Number a) {
            return isOdd(convert(a));
        }

        public default boolean isOdd(T a) {
            return !isZero(remainder(a, convert(2)));
        }
    }

    public static final class DecimalOperators implements ArithmeticOperators<BigDecimal> {
        public static final DecimalOperators INSTANCE = new DecimalOperators();
        public static final BigDecimal TWO = BigDecimal.ONE.add(BigDecimal.ONE);

        @Override
        public BigDecimal convert(Number a) {
            if (a instanceof BigFraction)
                return ((BigFraction) a).bigDecimalValue(BigDecimal.ROUND_HALF_EVEN);
            if (a instanceof BigDecimal)
                return (BigDecimal) a;
            if (a instanceof BigInteger)
                return new BigDecimal((BigInteger) a);
            if (a instanceof Float || a instanceof Double)
                return BigDecimal.valueOf(a.doubleValue());
            return BigDecimal.valueOf(a.longValue());
        }

        @Override
        public BigDecimal convertExact(Number a) throws ArithmeticException {
            if (a instanceof BigFraction)
                return ((BigFraction) a).bigDecimalValue();
            if (a instanceof BigDecimal)
                return (BigDecimal) a;
            if (a instanceof BigInteger)
                return new BigDecimal((BigInteger) a);
            // Don't current have a reliable way to convert non-integer floats
            // and double to decimal exactly
            if ((a instanceof Double || a instanceof Float) && a.doubleValue() % 1 != 0)
                throw new ArithmeticException();
            return BigDecimal.valueOf(a.longValue());
        }

        @Override
        public BigDecimal sum(BigDecimal a, BigDecimal b) {
            return a.add(b);
        }

        @Override
        public BigDecimal sumExact(BigDecimal a, BigDecimal b) throws ArithmeticException {
            return a.add(b);
        }

        @Override
        public BigDecimal difference(BigDecimal a, BigDecimal b) {
            return a.subtract(b);
        }

        @Override
        public BigDecimal differenceExact(BigDecimal a, BigDecimal b) throws ArithmeticException {
            return a.subtract(b);
        }

        @Override
        public BigDecimal product(BigDecimal a, BigDecimal b) {
            return a.multiply(b);
        }

        @Override
        public BigDecimal productExact(BigDecimal a, BigDecimal b) throws ArithmeticException {
            return a.multiply(b);
        }

        @Override
        public BigDecimal quotient(BigDecimal a, BigDecimal b) {
            return a.divide(b);
        }

        @Override
        public BigDecimal quotientExact(BigDecimal a, BigDecimal b) throws ArithmeticException {
            return a.divide(b);
        }

        @Override
        public boolean equals(BigDecimal a, BigDecimal b) {
            return false;
        }

        @Override
        public boolean isZero(BigDecimal a) {
            return a.equals(BigDecimal.ZERO);
        }

        @Override
        public int cmp(BigDecimal a, BigDecimal b) {
            return a.compareTo(b);
        }

        @Override
        public BigDecimal negate(BigDecimal a) {
            return a.negate();
        }

        @Override
        public int signum(BigDecimal a) {
            return a.signum();
        }

        @Override
        public BigDecimal abs(BigDecimal a) {
            return a.abs();
        }

        @Override
        public BigDecimal reciprocal(BigDecimal a) {
            return BigDecimal.ONE.divide(a);
        }

        @Override
        public BigDecimal remainder(BigDecimal divisor, BigDecimal dividend) {
            return divisor.remainder(dividend);
        }

        @Override
        public boolean isOdd(BigDecimal a) {
            return !isZero(a.remainder(TWO));
        }
    }

    public static final class BigIntegerOperators implements ArithmeticOperators<BigInteger> {
        public static final BigIntegerOperators INSTANCE = new BigIntegerOperators();
        public static final BigInteger TWO = BigInteger.valueOf(2);

        @Override
        public BigInteger convert(Number a) {
            if (a instanceof BigFraction) {
                BigFraction frac = (BigFraction) a;
                return frac.getNumerator().divide(frac.getDenominator());
            }
            if (a instanceof BigInteger)
                return (BigInteger) a;
            if (a instanceof BigDecimal)
                return ((BigDecimal) a).toBigInteger();
            return BigInteger.valueOf(a.longValue());
        }

        @Override
        public BigInteger convertExact(Number a) throws ArithmeticException {
            if (a instanceof BigFraction) {
                BigFraction frac = ((BigFraction) a).reduce();
                if (!frac.getDenominator().equals(BigInteger.ONE))
                    throw new ArithmeticException();
                return frac.getNumerator();
            }
            if (a instanceof BigInteger)
                return (BigInteger) a;
            if (a instanceof BigDecimal)
                return ((BigDecimal) a).toBigIntegerExact();
            if (a instanceof Double && a.doubleValue() % 1.0 != 0 || a instanceof Float && a.floatValue() % 1.0 != 0)
                throw new ArithmeticException();
            return BigInteger.valueOf(a.longValue());
        }

        @Override
        public BigInteger sum(BigInteger a, BigInteger b) {
            return a.add(b);
        }

        @Override
        public BigInteger sumExact(BigInteger a, BigInteger b) throws ArithmeticException {
            return a.add(b);
        }

        @Override
        public BigInteger difference(BigInteger a, BigInteger b) {
            return a.subtract(b);
        }

        @Override
        public BigInteger differenceExact(BigInteger a, BigInteger b) throws ArithmeticException {
            return a.subtract(b);
        }

        @Override
        public BigInteger product(BigInteger a, BigInteger b) {
            return a.multiply(b);
        }

        @Override
        public BigInteger productExact(BigInteger a, BigInteger b) throws ArithmeticException {
            return a.multiply(b);
        }

        @Override
        public BigInteger quotient(BigInteger a, BigInteger b) {
            return a.divide(b);
        }

        @Override
        public BigInteger quotientExact(BigInteger a, BigInteger b) throws ArithmeticException {
            BigInteger[] divMod = a.divideAndRemainder(b);
            if (!divMod[1].equals(BigInteger.ZERO))
                throw new ArithmeticException();
            return divMod[0];
        }

        @Override
        public boolean equals(BigInteger a, BigInteger b) {
            return a.equals(b);
        }

        @Override
        public boolean isZero(BigInteger a) {
            return a.equals(BigInteger.ZERO);
        }

        @Override
        public int cmp(BigInteger a, BigInteger b) {
            return a.compareTo(b);
        }

        @Override
        public BigInteger negate(BigInteger a) {
            return a.negate();
        }

        @Override
        public int signum(BigInteger a) {
            return a.signum();
        }

        @Override
        public BigInteger abs(BigInteger a) {
            return a.abs();
        }

        @Override
        public BigInteger reciprocal(BigInteger a) {
            if (a.equals(BigInteger.ONE))
                return a;
            // if(a.equals(BigInteger.ZERO)) return ???;
            return BigInteger.ZERO;
        }

        @Override
        public BigInteger remainder(BigInteger divisor, BigInteger dividend) {
            return divisor.remainder(dividend);
        }

        @Override
        public boolean isOdd(BigInteger a) {
            return !isZero(a.remainder(TWO));
        }
    }

    public static final class IntegerOperators implements ArithmeticOperators<Integer> {
        public static final IntegerOperators INSTANCE = new IntegerOperators();

        @Override
        public Integer convertExact(Number a) throws ArithmeticException {
            if (a instanceof BigFraction) {
                BigFraction frac = ((BigFraction) a).reduce();
                if (!frac.getDenominator().equals(BigInteger.ONE))
                    throw new ArithmeticException();
                return frac.getNumerator().intValueExact();
            }
            if (a instanceof BigDecimal)
                return convertExact(((BigDecimal) a).toBigIntegerExact());
            if (a instanceof BigInteger)
                return ((BigInteger) a).intValueExact();
            if (a instanceof Double && a.doubleValue() % 1.0 != 0 || a instanceof Float && a.floatValue() % 1.0 != 0)
                throw new ArithmeticException();
            if (a instanceof Long)
                return Math.toIntExact(a.longValue());
            return a.intValue();
        }

        @Override
        public Integer sum(Integer a, Integer b) {
            return Integer.sum(a, b);
        }

        @Override
        public Integer convert(Number a) {
            return a.intValue();
        }

        @Override
        public Integer sumExact(Integer a, Integer b) throws ArithmeticException {
            return Math.addExact(a, b);
        }

        @Override
        public Integer difference(Integer a, Integer b) {
            return a - b;
        }

        @Override
        public Integer differenceExact(Integer a, Integer b) throws ArithmeticException {
            return Math.subtractExact(a, b);
        }

        @Override
        public Integer product(Integer a, Integer b) {
            return a * b;
        }

        @Override
        public Integer productExact(Integer a, Integer b) throws ArithmeticException {
            return Math.multiplyExact(a, b);
        }

        @Override
        public Integer quotient(Integer a, Integer b) {
            return a / b;
        }

        @Override
        public Integer quotientExact(Integer a, Integer b) throws ArithmeticException {
            if (a % b != 0)
                throw new ArithmeticException("Cannot divide exactly");
            return a / b;
        }

        @Override
        public boolean equals(Integer a, Integer b) {
            return a.intValue() == b.intValue();
        }

        @Override
        public boolean isZero(Integer a) {
            return a.intValue() == 0;
        }

        @Override
        public int cmp(Integer a, Integer b) {
            return Integer.compare(a, b);
        }

        @Override
        public boolean ascending(Integer a, Integer b) {
            return a < b;
        }

        @Override
        public boolean descending(Integer a, Integer b) {
            return a > b;
        }

        @Override
        public Integer negate(Integer a) {
            return -a;
        }

        @Override
        public int signum(Integer a) {
            return Integer.signum(a);
        }

        @Override
        public Integer abs(Integer a) {
            return Math.abs(a);
        }

        @Override
        public Integer reciprocal(Integer a) {
            if (a == 1)
                return 1;
            // if(a == 0) return ???;
            return 0;
        }

        @Override
        public Integer remainder(Integer divisor, Integer dividend) {
            return divisor % dividend;
        }

        @Override
        public boolean isOdd(Integer a) {
            return (a & 1) == 1;
        }
    }

    public static final class LongOperators implements ArithmeticOperators<Long> {
        public static final LongOperators INSTANCE = new LongOperators();

        @Override
        public Long convert(Number a) {
            return a.longValue();
        }

        @Override
        public Long convertExact(Number a) throws ArithmeticException {
            if (a instanceof BigFraction) {
                BigFraction frac = ((BigFraction) a).reduce();
                if (!frac.getDenominator().equals(BigInteger.ONE))
                    throw new ArithmeticException();
                return frac.getNumerator().longValueExact();
            }
            if (a instanceof BigDecimal)
                return convertExact(((BigDecimal) a).longValueExact());
            if (a instanceof BigInteger)
                return ((BigInteger) a).longValueExact();
            if (a instanceof Double && a.doubleValue() % 1.0 != 0 || a instanceof Float && a.floatValue() % 1.0 != 0)
                throw new ArithmeticException();
            return a.longValue();
        }

        @Override
        public Long sum(Long a, Long b) {
            return a + b;
        }

        @Override
        public Long sumExact(Long a, Long b) throws ArithmeticException {
            return Math.addExact(a, b);
        }

        @Override
        public Long difference(Long a, Long b) {
            return a - b;
        }

        @Override
        public Long differenceExact(Long a, Long b) throws ArithmeticException {
            return Math.subtractExact(a, b);
        }

        @Override
        public Long product(Long a, Long b) {
            return a * b;
        }

        @Override
        public Long productExact(Long a, Long b) throws ArithmeticException {
            return Math.multiplyExact(a, b);
        }

        @Override
        public Long quotient(Long a, Long b) {
            return a / b;
        }

        @Override
        public Long quotientExact(Long a, Long b) throws ArithmeticException {
            if (a % b != 0)
                throw new ArithmeticException("Cannot divide exactly");
            return a / b;
        }

        @Override
        public boolean equals(Long a, Long b) {
            return a.longValue() == b.longValue();
        }

        @Override
        public boolean isZero(Long a) {
            return a.longValue() == 0;
        }

        @Override
        public int cmp(Long a, Long b) {
            return Long.compare(a, b);
        }

        @Override
        public boolean ascending(Long a, Long b) {
            return a < b;
        }

        @Override
        public boolean descending(Long a, Long b) {
            return a > b;
        }

        @Override
        public Long negate(Long a) {
            return -a;
        }

        @Override
        public int signum(Long a) {
            return Long.signum(a);
        }

        @Override
        public Long abs(Long a) {
            return Math.abs(a);
        }

        @Override
        public Long reciprocal(Long a) {
            if (a == 1)
                return 1L;
            // if(a == 0) return ???;
            return 0L;
        }

        @Override
        public Long remainder(Long divisor, Long dividend) {
            return divisor % dividend;
        }

        @Override
        public boolean isOdd(Long a) {
            return (a & 1) == 1;
        }

    }

    public static final class DoubleOperators implements ArithmeticOperators<Double> {
        public static final DoubleOperators INSTANCE = new DoubleOperators();

        @Override
        public Double convert(Number a) {
            return a.doubleValue();
        }

        @Override
        public Double convertExact(Number a) throws ArithmeticException {
            if (a instanceof Double || (a instanceof Long && Math.abs(a.longValue()) < (1 << 53))
                    || a instanceof Integer || a instanceof Short || a instanceof Byte)
                return a.doubleValue();
            // Don't have a good way to convert other types to double exactly
            throw new ArithmeticException();
        }

        @Override
        public Double sum(Double a, Double b) {
            return a + b;
        }

        @Override
        public Double sumExact(Double a, Double b) throws ArithmeticException {
            throw new ArithmeticException("Exact arithmetic not supported");
        }

        @Override
        public Double difference(Double a, Double b) {
            return a - b;
        }

        @Override
        public Double differenceExact(Double a, Double b) throws ArithmeticException {
            throw new ArithmeticException("Exact arithmetic not supported");
        }

        @Override
        public Double product(Double a, Double b) {
            return a * b;
        }

        @Override
        public Double productExact(Double a, Double b) throws ArithmeticException {
            throw new ArithmeticException("Exact arithmetic not supported");
        }

        @Override
        public Double quotient(Double a, Double b) {
            return a / b;
        }

        @Override
        public Double quotientExact(Double a, Double b) throws ArithmeticException {
            throw new ArithmeticException("Exact arithmetic not supported");
        }

        @Override
        public boolean equals(Double a, Double b) {
            return a.doubleValue() == b.doubleValue();
        }

        @Override
        public boolean isZero(Double a) {
            return a.doubleValue() == 0;
        }

        @Override
        public int cmp(Double a, Double b) {
            return Double.compare(a, b);
        }

        @Override
        public boolean ascending(Double a, Double b) {
            return a < b;
        }

        @Override
        public boolean descending(Double a, Double b) {
            return a > b;
        }

        @Override
        public Double negate(Double a) {
            return -a;
        }

        @Override
        public int signum(Double a) {
            return (int) Math.signum(a);
        }

        @Override
        public Double abs(Double a) {
            return Math.abs(a);
        }

        @Override
        public Double reciprocal(Double a) {
            return 1.0 / a;
        }

        @Override
        public Double remainder(Double divisor, Double dividend) {
            return divisor % dividend;
        }

        @Override
        public boolean isOdd(Double a) {
            return remainder(a, 2.0) == 1.0;
        }

    }

    public static final class FloatOperators implements ArithmeticOperators<Float> {
        public static final FloatOperators INSTANCE = new FloatOperators();

        @Override
        public Float convert(Number a) {
            return a.floatValue();
        }

        @Override
        public Float convertExact(Number a) throws ArithmeticException {
            if (a instanceof Float || (a instanceof Integer && Math.abs(a.intValue()) < (1 << 24)) || a instanceof Short
                    || a instanceof Byte)
                return a.floatValue();

            // Don't have a good way to convert other types to float exactly
            throw new ArithmeticException();
        }

        @Override
        public Float sum(Float a, Float b) {
            return a + b;
        }

        @Override
        public Float sumExact(Float a, Float b) throws ArithmeticException {
            throw new ArithmeticException("Exact arithmetic not supported");
        }

        @Override
        public Float difference(Float a, Float b) {
            return a - b;
        }

        @Override
        public Float differenceExact(Float a, Float b) throws ArithmeticException {
            throw new ArithmeticException("Exact arithmetic not supported");
        }

        @Override
        public Float product(Float a, Float b) {
            return a * b;
        }

        @Override
        public Float productExact(Float a, Float b) throws ArithmeticException {
            throw new ArithmeticException("Exact arithmetic not supported");
        }

        @Override
        public Float quotient(Float a, Float b) {
            return a / b;
        }

        @Override
        public Float quotientExact(Float a, Float b) throws ArithmeticException {
            throw new ArithmeticException("Exact arithmetic not supported");
        }

        @Override
        public boolean equals(Float a, Float b) {
            return a.floatValue() == b.floatValue();
        }

        @Override
        public boolean isZero(Float a) {
            return a.floatValue() == 0;
        }

        @Override
        public int cmp(Float a, Float b) {
            return Float.compare(a, b);
        }

        @Override
        public boolean ascending(Float a, Float b) {
            return a < b;
        }

        @Override
        public boolean descending(Float a, Float b) {
            return a > b;
        }

        @Override
        public Float negate(Float a) {
            return -a;
        }

        @Override
        public int signum(Float a) {
            return (int) Math.signum(a);
        }

        @Override
        public Float abs(Float a) {
            return Math.abs(a);
        }

        @Override
        public Float reciprocal(Float a) {
            return 1.0f / a;
        }

        @Override
        public Float remainder(Float divisor, Float dividend) {
            return divisor % dividend;
        }

        @Override
        public boolean isOdd(Float a) {
            return (a % 2.0f) == 1.0f;
        }
    }

    public static final class BigFractionOperators implements ArithmeticOperators<BigFraction> {
        public static final BigFractionOperators INSTANCE = new BigFractionOperators();

        public static BigFraction bigDecimalToBigFraction(BigDecimal bd) {
            int scale = bd.scale();

            // If scale >= 0 then the value is bd.unscaledValue() / 10^scale
            if (scale >= 0)
                return new BigFraction(bd.unscaledValue(), BigInteger.TEN.pow(scale));

            // If scale < 0 then the value is bd.unscaledValue() * 10^-scale
            return new BigFraction(bd.unscaledValue().multiply(BigInteger.TEN.pow(-scale)));
        }
        @Override
        public BigFraction convert(Number a) {
            if(a instanceof BigFraction)
                return (BigFraction) a;
            if(a instanceof BigDecimal)
                return bigDecimalToBigFraction((BigDecimal) a);
            if (a instanceof Double || a instanceof Float)
                return new BigFraction(a.doubleValue());
            return new BigFraction(BigIntegerOperators.INSTANCE.convert(a));
        }

        @Override
        public BigFraction convertExact(Number a) throws ArithmeticException {
            if (a instanceof BigFraction)
                return (BigFraction) a;
            if (a instanceof BigDecimal)
                return bigDecimalToBigFraction((BigDecimal) a);

            // Accept anything we can convert into a BigInteger without loss of
            // information
            return new BigFraction(BigIntegerOperators.INSTANCE.convertExact(a));
        }

        @Override
        public BigFraction sum(BigFraction a, BigFraction b) {
            return a.add(b);
        }

        @Override
        public BigFraction sumExact(BigFraction a, BigFraction b) throws ArithmeticException {
            return a.add(b);
        }

        @Override
        public BigFraction difference(BigFraction a, BigFraction b) {
            return a.subtract(b);
        }

        @Override
        public BigFraction differenceExact(BigFraction a, BigFraction b) throws ArithmeticException {
            return a.subtract(b);
        }

        @Override
        public BigFraction product(BigFraction a, BigFraction b) {
            return a.multiply(b);
        }

        @Override
        public BigFraction productExact(BigFraction a, BigFraction b) throws ArithmeticException {
            return a.multiply(b);
        }

        @Override
        public BigFraction quotient(BigFraction a, BigFraction b) {
            return a.divide(b);
        }

        @Override
        public BigFraction quotientExact(BigFraction a, BigFraction b) throws ArithmeticException {
            return a.divide(b);
        }

        @Override
        public boolean equals(BigFraction a, BigFraction b) {
            return a.equals(b);
        }

        @Override
        public boolean isZero(BigFraction a) {
            return a.equals(BigFraction.ZERO);
        }

        @Override
        public int cmp(BigFraction a, BigFraction b) {
            return a.compareTo(b);
        }

        @Override
        public BigFraction negate(BigFraction a) {
            return a.negate();
        }

        @Override
        public int signum(BigFraction a) {
            return a.getDenominator().signum() * a.getNumerator().signum();
        }

        @Override
        public BigFraction reciprocal(BigFraction a) {
            return a.reciprocal();
        }

        @Override
        public BigFraction remainder(BigFraction divisor, BigFraction dividend) {
            BigFraction tmp = divisor.divide(dividend);
            return new BigFraction(tmp.getNumerator().remainder(tmp.getDenominator()));
        }

        @Override
        public boolean isOdd(BigFraction a) {
            BigFraction half = a.divide(BigFraction.TWO);
            return !half.getDenominator().equals(BigFraction.ONE);
        }
    }

    public static final class ArithmeticOperatorsWithFallback<A extends Number, B extends Number, C extends Number>
            implements ArithmeticOperators<Number> {
        // Exact Double operations will use BigDecimal
        public static final ArithmeticOperatorsWithFallback<Double, BigDecimal, BigDecimal> DOUBLE_OR_DECIMAL = new ArithmeticOperatorsWithFallback<>(
                DoubleOperators.INSTANCE, DecimalOperators.INSTANCE, DecimalOperators.INSTANCE);
        // Exact Float operations will use BigDecimal
        public static final ArithmeticOperatorsWithFallback<Float, BigDecimal, BigDecimal> FLOAT_OR_DECIMAL = new ArithmeticOperatorsWithFallback<>(
                FloatOperators.INSTANCE, DecimalOperators.INSTANCE, DecimalOperators.INSTANCE);

        // Exact BigInteger division can fallback on fraction if not dividing
        // evenly
        public static final ArithmeticOperatorsWithFallback<BigInteger, BigInteger, BigFraction> BIGINTEGER_OR_BIGFRACTION = new ArithmeticOperatorsWithFallback<>(
                BigIntegerOperators.INSTANCE, BigIntegerOperators.INSTANCE, BigFractionOperators.INSTANCE);

        // Exact Long add / subtract / multiple can overflow into BigInteger or
        // divide using BigFraction
        public static final ArithmeticOperatorsWithFallback<Long, BigInteger, BigFraction> LONG = new ArithmeticOperatorsWithFallback<>(
                LongOperators.INSTANCE, BigIntegerOperators.INSTANCE, BigFractionOperators.INSTANCE);
        // Exact Integer add / subtract / multiple can overflow into Long or
        // divide using BigFraction
        public static final ArithmeticOperatorsWithFallback<Integer, Number, BigFraction> INTEGER = new ArithmeticOperatorsWithFallback<>(
                IntegerOperators.INSTANCE, LONG,
                BigFractionOperators.INSTANCE);
        public static final ArithmeticOperatorsWithFallback<Double, Double, BigDecimal> DOUBLE = new ArithmeticOperatorsWithFallback<>(
                DoubleOperators.INSTANCE, DoubleOperators.INSTANCE, DecimalOperators.INSTANCE);
        public static final ArithmeticOperatorsWithFallback<Float, Float, Double> FLOAT = new ArithmeticOperatorsWithFallback<>(
                FloatOperators.INSTANCE, FloatOperators.INSTANCE, DoubleOperators.INSTANCE);

        public final ArithmeticOperators<A> delegate;
        public final ArithmeticOperators<B> fallback;
        public final ArithmeticOperators<C> divisionFallback;

        public ArithmeticOperatorsWithFallback(ArithmeticOperators<A> delegate, ArithmeticOperators<B> fallback,
                ArithmeticOperators<C> fraction) {
            super();
            this.delegate = delegate;
            this.fallback = fallback;
            this.divisionFallback = fraction;
        }

        @Override
        public Number convert(Number a) {
            try {
                return delegate.convertExact(a);
            } catch (ArithmeticException e) {
                return fallback.convert(a);
            }
        }

        @Override
        public Number convertExact(Number a) throws ArithmeticException {
            try {
                return delegate.convertExact(a);
            } catch (ArithmeticException e) {
                return fallback.convertExact(a);
            }
        }

        @Override
        public Number sum(Number a, Number b) {
            try {
                return delegate.sumExact(delegate.convertExact(a), delegate.convertExact(b));
            } catch (ArithmeticException e) {
                return fallback.sum(fallback.convert(a), fallback.convert(b));
            }
        }

        @Override
        public Number sumExact(Number a, Number b) throws ArithmeticException {
            try {
                return delegate.sumExact(delegate.convertExact(a), delegate.convertExact(b));
            } catch (ArithmeticException e) {
                return fallback.sumExact(fallback.convertExact(a), fallback.convertExact(b));
            }
        }

        @Override
        public Number difference(Number a, Number b) {
            try {
                return delegate.differenceExact(delegate.convertExact(a), delegate.convertExact(b));
            } catch (ArithmeticException e) {
                return fallback.difference(fallback.convert(a), fallback.convert(b));
            }
        }

        @Override
        public Number differenceExact(Number a, Number b) throws ArithmeticException {
            try {
                return delegate.differenceExact(delegate.convertExact(a), delegate.convertExact(b));
            } catch (ArithmeticException e) {
                return fallback.differenceExact(fallback.convertExact(a), fallback.convertExact(b));
            }
        }

        @Override
        public Number product(Number a, Number b) {
            try {
                return delegate.productExact(delegate.convertExact(a), delegate.convertExact(b));
            } catch (ArithmeticException e) {
                return fallback.product(fallback.convert(a), fallback.convert(b));
            }
        }

        @Override
        public Number productExact(Number a, Number b) throws ArithmeticException {
            try {
                return delegate.productExact(delegate.convertExact(a), delegate.convertExact(b));
            } catch (ArithmeticException e) {
                return fallback.productExact(fallback.convertExact(a), fallback.convertExact(b));
            }
        }

        @Override
        public Number quotient(Number a, Number b) {
            try {
                return delegate.quotientExact(delegate.convertExact(a), delegate.convertExact(b));
            } catch (ArithmeticException e) {
                return divisionFallback.quotient(divisionFallback.convert(a), divisionFallback.convert(b));
            }
        }

        @Override
        public Number quotientExact(Number a, Number b) throws ArithmeticException {
            try {
                return delegate.quotientExact(delegate.convertExact(a), delegate.convertExact(b));
            } catch (ArithmeticException e) {
                return divisionFallback.quotientExact(divisionFallback.convertExact(a),
                        divisionFallback.convertExact(b));
            }
        }

        @Override
        public boolean equals(Number a, Number b) {
            try {
                return delegate.equals(delegate.convertExact(a), delegate.convertExact(b));
            } catch (ArithmeticException e) {
                return fallback.equals(fallback.convert(a), fallback.convert(b));
            }
        }

        @Override
        public boolean isZero(Number a) {
            try {
                return delegate.isZero(delegate.convertExact(a));
            } catch (ArithmeticException e) {
                return fallback.isZero(fallback.convert(a));
            }
        }

        @Override
        public int cmp(Number a, Number b) {
            try {
                return delegate.cmp(delegate.convertExact(a), delegate.convertExact(b));
            } catch (ArithmeticException e) {
                return fallback.cmp(fallback.convert(a), fallback.convert(b));
            }
        }

        @Override
        public boolean ascending(Number a, Number b) {
            return delegate.ascendingConverted(a, b);
        }

        @Override
        public boolean descending(Number a, Number b) {
            return delegate.descendingConverted(a, b);
        }

        @Override
        public Number negate(Number a) {
            try {
                return delegate.negate(delegate.convertExact(a));
            } catch (ArithmeticException e) {
                return fallback.negate(fallback.convert(a));
            }
        }

        @Override
        public int signum(Number a) {
            try {
                return delegate.signum(delegate.convertExact(a));
            } catch (ArithmeticException e) {
                return fallback.signum(fallback.convert(a));
            }
        }

        @Override
        public Number abs(Number a) {
            try {
                return delegate.abs(delegate.convertExact(a));
            } catch (ArithmeticException e) {
                return fallback.abs(fallback.convert(a));
            }
        }

        @Override
        public Number reciprocal(Number a) {
            return divisionFallback.reciprocalConverted(a);
        }

        @Override
        public boolean isOdd(Number a) {
            try {
                return delegate.isOdd(delegate.convertExact(a));
            } catch (ArithmeticException e) {
                return fallback.isOdd(fallback.convert(a));
            }
        }

        @Override
        public Number remainder(Number a, Number b) {
            try {
                return delegate.remainder(delegate.convertExact(a), delegate.convertExact(b));
            } catch (ArithmeticException e) {
                return divisionFallback.remainder(divisionFallback.convert(a), divisionFallback.convert(b));
            }
        }

        public static ArithmeticOperators<? extends Number> forOperand(Number a) {
            if (a instanceof BigDecimal)
                return DecimalOperators.INSTANCE;
            if (a instanceof BigFraction)
                return BigFractionOperators.INSTANCE;
            if (a instanceof BigInteger)
                return ArithmeticOperatorsWithFallback.BIGINTEGER_OR_BIGFRACTION;
            if (a instanceof Long)
                return ArithmeticOperatorsWithFallback.LONG;
            if (a instanceof Integer || a instanceof Short || a instanceof Byte)
                return ArithmeticOperatorsWithFallback.INTEGER;
            if (a instanceof Double)
                return ArithmeticOperatorsWithFallback.DOUBLE;
            if (a instanceof Float)
                return ArithmeticOperatorsWithFallback.FLOAT;
            throw new Error("Unable to determine operators for " + a.getClass().getSimpleName());
        }

        public static ArithmeticOperators<? extends Number> forOperands(Number a, Number b) {
            return forOperands(a, b, false);
        }
        public static ArithmeticOperators<? extends Number> forOperands(Number a, Number b, boolean swapped) {
            if (a instanceof BigDecimal)
                return DecimalOperators.INSTANCE;
            if (a instanceof BigFraction) {
                if(b instanceof BigFraction || b instanceof BigInteger || b instanceof Long || b instanceof Integer || b instanceof Short || b instanceof Byte)
                    return BigFractionOperators.INSTANCE;
                if(b instanceof Double || b instanceof Float)
                    return DecimalOperators.INSTANCE;
            } else if(a instanceof BigInteger) {
                if(b instanceof BigInteger || b instanceof Long || b instanceof Integer || b instanceof Short || b instanceof Byte)
                    return ArithmeticOperatorsWithFallback.BIGINTEGER_OR_BIGFRACTION;
                if(b instanceof Double || b instanceof Float)
                    return DecimalOperators.INSTANCE;
            } else if (a instanceof Long
                    && (b instanceof Long || b instanceof Integer || b instanceof Short || b instanceof Byte)) {
                    return ArithmeticOperatorsWithFallback.LONG;
            } else if ((a instanceof Integer || a instanceof Short || a instanceof Byte)
                    && (b instanceof Integer || b instanceof Short || b instanceof Byte)) {
                return ArithmeticOperatorsWithFallback.INTEGER;
            } else if (a instanceof Double && (b instanceof Double || b instanceof Float || b instanceof Long
                    || b instanceof Integer || b instanceof Short || b instanceof Byte)) {
                return ArithmeticOperatorsWithFallback.DOUBLE;
            } else if (a instanceof Float
                    && (b instanceof Float || b instanceof Integer || b instanceof Short || b instanceof Byte)) {
                return ArithmeticOperatorsWithFallback.FLOAT;
            }
            if (swapped)
                throw new Error("Unable to determine operators for " + a.getClass().getSimpleName() + " and "
                        + b.getClass().getSimpleName());
            return forOperands(b, a, true);
        }
    }

    public static final class ExactArithmeticOperators implements ArithmeticOperators<Number> {
        public static final ExactArithmeticOperators INSTANCE = new ExactArithmeticOperators();

        @Override
        public Number convert(Number a) {
            return a;
        }

        @Override
        public Number convertExact(Number a) throws ArithmeticException {
            return a;
        }

        public static boolean haveBigInteger(NumberLiteral a, NumberLiteral b) {
            return a.number instanceof BigInteger || b.number instanceof BigInteger;
        }

        public static boolean haveLongOrDouble(NumberLiteral a, NumberLiteral b) {
            return a.number instanceof Long || b.number instanceof Long || a.number instanceof Double
                    || b.number instanceof Double;
        }

        public static boolean haveAnyDecimalOrFloat(NumberLiteral a, NumberLiteral b) {
            return a.number instanceof BigDecimal || b.number instanceof BigDecimal
                    || a.number instanceof Double && a.number.doubleValue() % 1 != 0
                    || b.number instanceof Double && b.number.doubleValue() % 1 != 0
                    || a.number instanceof Float && a.number.floatValue() % 1 != 0
                    || b.number instanceof Float && b.number.floatValue() % 1 != 0;
        }

        ArithmeticOperators<? extends Number> selectOperators(Number a, Number b) {
            return ArithmeticOperatorsWithFallback.forOperands(a, b);
        }

        @Override
        public Number sum(Number a, Number b) {
            return ArithmeticOperatorsWithFallback.forOperands(a, b).sumConverted(a, b);
        }

        @Override
        public Number sumExact(Number a, Number b) throws ArithmeticException {
            return ArithmeticOperatorsWithFallback.forOperands(a, b).sumConverted(a, b);
        }

        @Override
        public Number difference(Number a, Number b) {
            return ArithmeticOperatorsWithFallback.forOperands(a, b).differenceConverted(a, b);
        }

        @Override
        public Number differenceExact(Number a, Number b) throws ArithmeticException {
            return ArithmeticOperatorsWithFallback.forOperands(a, b).differenceConverted(a, b);
        }

        @Override
        public Number product(Number a, Number b) {
            return ArithmeticOperatorsWithFallback.forOperands(a, b).productConverted(a, b);
        }

        @Override
        public Number productExact(Number a, Number b) throws ArithmeticException {
            return ArithmeticOperatorsWithFallback.forOperands(a, b).productConverted(a, b);
        }

        @Override
        public Number quotient(Number a, Number b) {
            return ArithmeticOperatorsWithFallback.forOperands(a, b).quotientConverted(a, b);
        }

        @Override
        public Number quotientExact(Number a, Number b) throws ArithmeticException {
            return ArithmeticOperatorsWithFallback.forOperands(a, b).quotientConverted(a, b);
        }

        @Override
        public boolean equals(Number a, Number b) {
            return ArithmeticOperatorsWithFallback.forOperands(a, b).equalsConverted(a, b);
        }

        @Override
        public boolean isZero(Number a) {
            return ArithmeticOperatorsWithFallback.forOperand(a).zeroConverted(a);
        }

        @Override
        public int cmp(Number a, Number b) {
            return ArithmeticOperatorsWithFallback.forOperands(a, b).cmpConverted(a, b);
        }
        
        @Override
        public boolean ascending(Number a, Number b) {
            return ArithmeticOperatorsWithFallback.forOperands(a, b).ascendingConverted(a, b);
        }

        @Override
        public boolean descending(Number a, Number b) {
            return ArithmeticOperatorsWithFallback.forOperands(a, b).descendingConverted(a, b);
        }

        @Override
        public Number negate(Number a) {
            return ArithmeticOperatorsWithFallback.forOperand(a).negateConverted(a);
        }

        @Override
        public int signum(Number a) {
            return ArithmeticOperatorsWithFallback.forOperand(a).signumConverted(a);
        }

        @Override
        public Number abs(Number a) {
            return ArithmeticOperatorsWithFallback.forOperand(a).absConverted(a);
        }

        @Override
        public Number reciprocal(Number a) {
            return ArithmeticOperatorsWithFallback.forOperand(a).reciprocalConverted(a);
        }

        @Override
        public boolean isOdd(Number a) {
            return ArithmeticOperatorsWithFallback.forOperand(a).isOddConverted(a);
        }

        @Override
        public Number remainder(Number a, Number b) {
            return ArithmeticOperatorsWithFallback.forOperands(a, b).remainderConverted(a, b);
        }

    }
    public int intValue() {
        return number.intValue();
    }

    public long longValue() {
        return number.longValue();
    }

    public float floatValue() {
        return number.floatValue();
    }

    public double doubleValue() {
        return number.doubleValue();
    }
    @Override
    public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
        return visitor.numberLiteral(this);
    }

    public Number getNumber() {
    	return this.number;
    }

    @Override
    public Precedence getPrecedence() {
    	return Precedence.ATOM;
    }

    @Override
    public List<BadExpr> getProblems() {
    	return List.nil();
    }

    @Override
    public String toString() {
        return this.source;
    }

    @Override
    public void toSource(StringBuffer sb) {
        sb.append(this.source);
    }

    public boolean isNegative() {
        return ExactArithmeticOperators.INSTANCE.isNegative(number);
    }

    public boolean isOdd() {
        return ExactArithmeticOperators.INSTANCE.isOdd(number);
    }
}
