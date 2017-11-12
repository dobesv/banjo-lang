package banjo.expr.token;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Objects;

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

	static Number negateNumber(Number num) {
		if(num instanceof BigDecimal)
	        return Objects.requireNonNull(((BigDecimal)num).negate());
		if(num instanceof BigInteger)
	        return Objects.requireNonNull(((BigInteger)num).negate());
		if(num instanceof Long) return Long.valueOf(-num.longValue());
        if (num instanceof Byte)
            return Byte.valueOf((byte) -num.shortValue());
		if(num instanceof Short) return Short.valueOf((short)-num.shortValue());
		if(num instanceof Integer) return Integer.valueOf(-num.intValue());
		if(num instanceof Float) return Float.valueOf(-num.floatValue());
		if(num instanceof Double) return Double.valueOf(-num.doubleValue());
		throw new Error("Unexpected number subclass: "+num.getClass());
	}

    static int signum(Number num) {
        if (num instanceof BigDecimal)
            return Objects.requireNonNull(((BigDecimal) num).signum());
        if (num instanceof BigInteger)
            return Objects.requireNonNull(((BigInteger) num).signum());
        if (num instanceof Long)
            return Long.signum(num.longValue());
        if (num instanceof Byte)
            return Integer.signum(num.intValue());
        if (num instanceof Short)
            return Integer.signum(num.intValue());
        if (num instanceof Integer)
            return Integer.signum(num.intValue());
        if (num instanceof Float)
            return Integer.signum(Float.compare(num.floatValue(), 0.0f));
        if (num instanceof Double)
            return Integer.signum(Double.compare(num.doubleValue(), 0.0));
        throw new Error("Unexpected number subclass: " + num.getClass());
    }

    static boolean isNegative(Number num) {
        return signum(num) == -1;
    }

    public boolean isNegative() {
        return isNegative(number);
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
        return new NumberLiteral(ranges, indentColumn, negateNumber(number),
                source.charAt(0) == '-' ? source.substring(1) : "-" + source);
    }

    public CoreExpr absValue(Set<SourceFileRange> ranges) {
        return isNegative() ? negate(ranges) : this;
    }

    public int signum() {
        return signum(this.number);
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

    public static NumberLiteral int32Sum(NumberLiteral a, NumberLiteral b) {
        return fromNumber(Integer.valueOf(a.intValue() + b.intValue()));
    }

    public int intValue() {
        return number.intValue();
    }

    public static NumberLiteral int64Sum(NumberLiteral a, NumberLiteral b) {
        return fromNumber(Long.valueOf(a.longValue() + b.longValue()));
    }

    private long longValue() {
        return number.longValue();
    }

    public static NumberLiteral float32Sum(NumberLiteral a, NumberLiteral b) {
        return fromNumber(Float.valueOf(a.floatValue() + b.floatValue()));
    }

    public float floatValue() {
        return number.floatValue();
    }

    public static NumberLiteral float64Sum(NumberLiteral a, NumberLiteral b) {
        return fromNumber(Double.valueOf(a.doubleValue() + b.doubleValue()));
    }

    public double doubleValue() {
        return number.doubleValue();
    }

    public static NumberLiteral integerSum(NumberLiteral a, NumberLiteral b) {
        return fromNumber(a.toBigInteger().add(b.toBigInteger()));
    }

    public static NumberLiteral decimalSum(NumberLiteral a, NumberLiteral b) {
        return fromNumber(a.toBigDecimal().add(b.toBigDecimal()));
    }

    public static BigDecimal toBigDecimal(Number a) {
        if (a instanceof BigDecimal)
            return (BigDecimal) a;
        if (a instanceof BigInteger)
            return new BigDecimal((BigInteger) a);
        if (a instanceof Float || a instanceof Double)
            return BigDecimal.valueOf(a.doubleValue());
        return BigDecimal.valueOf(a.longValue());
    }

    public BigDecimal toBigDecimal() {
        return toBigDecimal(number);
    }

    public static BigInteger toBigInteger(Number a) {
        if (a instanceof BigInteger)
            return (BigInteger) a;
        if (a instanceof BigDecimal)
            return ((BigDecimal) a).toBigInteger();
        return BigInteger.valueOf(a.longValue());
    }

    public BigInteger toBigInteger() {
        return toBigInteger(number);
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
}
