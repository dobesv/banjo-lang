package banjo.expr.token;

import static java.util.Objects.requireNonNull;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Objects;

import banjo.expr.BadExpr;
import banjo.expr.core.BaseCoreExprVisitor;
import banjo.expr.core.Call;
import banjo.expr.core.CoreExpr;
import banjo.expr.core.CoreExprAlgebra;
import banjo.expr.core.CoreExprVisitor;
import banjo.expr.source.Operator;
import banjo.expr.source.Precedence;
import banjo.expr.source.SourceExpr;
import banjo.expr.source.SourceExprAlgebra;
import banjo.expr.source.SourceExprVisitor;
import banjo.expr.util.SourceFileRange;
import banjo.expr.util.SourceNumber;
import fj.Ord;
import fj.data.List;
import fj.data.Set;


public class NumberLiteral extends AbstractAtom implements Atom {
	public static final Ord<NumberLiteral> ORD = Ord.stringOrd.contramap((NumberLiteral n) -> n.number.toString());

	private final Number number;

	public NumberLiteral(SourceFileRange sfr, int indentColumn, Number number) {
		this(Set.single(SourceFileRange.ORD, sfr), indentColumn, number);
	}
	public NumberLiteral(Set<SourceFileRange> ranges, int indentColumn, Number number) {
		super(ranges, indentColumn);
		this.number = requireNonNull(number);
	}

	public NumberLiteral(Number n) {
		this(SourceFileRange.EMPTY_SET, 0, n);
	}

	public Number getNumber() {
		return this.number;
	}

	@Override
	public String toString() {
		return Objects.requireNonNull(this.number.toString());
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append(this.toString());
	}

	@Override
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.numberLiteral(this);
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
		if(num instanceof Byte) return Short.valueOf((short)-num.shortValue());
		if(num instanceof Short) return Short.valueOf((short)-num.shortValue());
		if(num instanceof Integer) return Integer.valueOf(-num.intValue());
		if(num instanceof Float) return Float.valueOf(-num.floatValue());
		if(num instanceof Double) return Double.valueOf(-num.doubleValue());
		if(num instanceof SourceNumber) {
			SourceNumber sn = (SourceNumber) num;
			return new SourceNumber("-"+sn.toString(), negateNumber(sn.getValue()));
		}
		throw new Error("Unexpected number subclass: "+num.getClass());
	}

	@Override
	public List<BadExpr> getProblems() {
		return List.nil();
	}

	@Override
	public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
		return visitor.numberLiteral(getSourceFileRanges(), number);
	}

	@Override
	public <T> T acceptVisitor(SourceExprAlgebra<T> visitor) {
		return visitor.numberLiteral(getSourceFileRanges(), number);
	}

	@Override
	public <T> T acceptVisitor(TokenVisitor<T> parser) {
		return parser.numberLiteral(getSourceFileRanges().toStream().head().getFileRange(), indentColumn, number);
	}

	public CoreExpr toConstructionExpression() {
		CoreExpr ctor;
		Number n = getNumber();
		if(n instanceof SourceNumber) n = ((SourceNumber) n).getValue();
		if(n instanceof BigInteger) {
			BigInteger bi = (BigInteger)n;
			int signum = bi.signum();
			if(bi.equals(BigInteger.ZERO)) {
				return Identifier.ZERO;
			}
			if(bi.equals(BigInteger.ONE)) {
				return Identifier.ONE;
			}

			final boolean negative = signum == -1;
			if(negative) {
				bi = bi.abs();
			}
			// 10 = 5 + 5
			// 5 = 2 + 2 + 1
			// 2 = 1 + 1
			boolean odd = bi.testBit(0);
			NumberLiteral half = new NumberLiteral(bi.shiftRight(1));
			ctor = Call.binaryOp(half, Operator.ADD, half);
			if(odd) ctor = Call.binaryOp(ctor, Operator.ADD, new Identifier("1"));
			if(negative) ctor = Call.unaryOp(ctor, Operator.NEGATE);
		} else if(n instanceof Integer) {
			int i = n.intValue();
			if(i == 0) {
				return new Identifier("0");
			}
			if(i == 1) {
				return new Identifier("1");
			}
			boolean negative = (i < 0);
			if(negative) i = -i;
			boolean odd = (i&1) == 1;
			NumberLiteral half = new NumberLiteral(i >> 1);
			ctor = Call.binaryOp(half, Operator.ADD, half);
			if(odd) ctor = Call.binaryOp(ctor, Operator.ADD, new Identifier("1"));
			if(negative) ctor = Call.unaryOp(ctor, Operator.NEGATE);
		} else if(n instanceof Long) {
			long i = n.longValue();
			if(i == 0) {
				return new Identifier("0");
			}
			if(i == 1) {
				return new Identifier("1");
			}
			boolean negative = (i < 0);
			if(negative) i = -i;
			boolean odd = (i&1) == 1;
			NumberLiteral half = new NumberLiteral(i >> 1);
			ctor = Call.binaryOp(half, Operator.ADD, half);
			if(odd) ctor = Call.binaryOp(ctor, Operator.ADD, new Identifier("1"));
			if(negative) ctor = Call.unaryOp(ctor, Operator.NEGATE);
		} else if(n instanceof Double) {
			double d = n.doubleValue();
			if(d == 0) {
				return Identifier.ZERO;
			}
			if(d == 1) {
				return Identifier.ONE;
			}
			if(d == Double.NaN) {
				return Identifier.NAN;
			}
			if(d == Double.NEGATIVE_INFINITY) {
				return Call.unaryOp(Identifier.INFINITY, Operator.NEGATE);
			}
			if(d == Double.POSITIVE_INFINITY) {
				return Identifier.INFINITY;
			}
			boolean negative = (d < 0);
			if(negative) d = -d;
			int exp = Math.getExponent(d);
			long base = Double.doubleToLongBits(d) & 0x000fffffffffffffL;

			ctor = new NumberLiteral(base).toConstructionExpression();
			if(exp != 0) ctor = Call.binaryOp(ctor, Operator.POW, new NumberLiteral(exp));
			if(negative) ctor = Call.unaryOp(ctor, Operator.NEGATE);
		} else throw new Error("TODO: "+n.getClass().getSimpleName());
		return ctor;
	}
	public SourceExpr negate(Set<SourceFileRange> ranges) {
	    return new NumberLiteral(ranges, indentColumn, negateNumber(number));
    }

}
