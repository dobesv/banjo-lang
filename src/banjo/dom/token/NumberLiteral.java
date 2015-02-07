package banjo.dom.token;

import static banjo.parser.util.Check.nonNull;

import java.math.BigDecimal;
import java.math.BigInteger;

import banjo.dom.BadExpr;
import banjo.dom.Expr;
import banjo.dom.core.BaseCoreExprVisitor;
import banjo.dom.core.Call;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.CoreExprAlgebra;
import banjo.dom.core.CoreExprVisitor;
import banjo.dom.source.Operator;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExprAlgebra;
import banjo.dom.source.SourceExprVisitor;
import banjo.parser.util.SourceFileRange;
import banjo.util.SourceNumber;
import fj.data.List;


public class NumberLiteral extends AbstractAtom implements Atom {
	private final Number number;
	private final String suffix;

	public NumberLiteral(SourceFileRange sfr, Number number, String suffix) {
		this(List.single(sfr), number, suffix);
	}
	public NumberLiteral(List<SourceFileRange> ranges, Number number, String suffix) {
		super(number.hashCode() ^ suffix.hashCode(), ranges);
		this.number = number;
		this.suffix = suffix;
	}

	public NumberLiteral(Number n) {
		this(List.nil(), n, "");
	}

	public Number getNumber() {
		return this.number;
	}

	@Override
	public String toString() {
		return nonNull(this.number.toString()+this.suffix);
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append(this.toString());
		sb.append(suffix);
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
		return nonNull(x.acceptVisitor(new BaseCoreExprVisitor<Boolean>() {
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
	public int compareTo(Expr o) {
		if(this == o)
			return 0;
		if(o == null) return -1;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final NumberLiteral other = (NumberLiteral) o;
			if(cmp == 0) cmp = number.getClass().getName().compareTo(other.number.getClass().getName());
			if(cmp == 0) cmp = suffix.compareTo(other.suffix);
			if(cmp == 0) {
				if(number instanceof SourceNumber) return ((SourceNumber)number).compareTo((SourceNumber)other.number);
				if(number instanceof BigDecimal) return ((BigDecimal)number).compareTo((BigDecimal)other.number);
				if(number instanceof BigInteger) return ((BigInteger)number).compareTo((BigInteger)other.number);
				if(number instanceof Long) return ((Long)number).compareTo((Long)other.number);
				if(number instanceof Integer) return ((Integer)number).compareTo((Integer)other.number);
			}
		}
		return cmp;
	}

	static Number negateNumber(Number num) {
		if(num instanceof BigDecimal) return nonNull(((BigDecimal)num).negate());
		if(num instanceof BigInteger) return nonNull(((BigInteger)num).negate());
		if(num instanceof Long) return new Long(-((Long)num).longValue());
		if(num instanceof Integer) return new Integer(-((Integer)num).intValue());
		throw new Error("Unexpected number subclass: "+num.getClass());
	}

	@Override
	public List<BadExpr> getProblems() {
		return List.nil();
	}

	@Override
	public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
		return visitor.numberLiteral(getSourceFileRanges(), number, suffix);
	}

	@Override
	public <T> T acceptVisitor(SourceExprAlgebra<T> visitor) {
		return visitor.numberLiteral(getSourceFileRanges(), number, suffix);
	}

	@Override
	public <T> T acceptVisitor(TokenVisitor<T> parser) {
		return parser.numberLiteral(getSourceFileRanges().head().getFileRange(), number, suffix);
	}

	public String getSuffix() {
		return suffix;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!super.equals(obj))
			return false;
		if (!(obj instanceof NumberLiteral))
			return false;
		NumberLiteral other = (NumberLiteral) obj;
		if (!number.equals(other.number))
			return false;
		if (!suffix.equals(other.suffix))
			return false;
		return true;
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
			ctor = Call.binaryOp(half, Operator.PLUS, half);
			if(odd) ctor = Call.binaryOp(ctor, Operator.PLUS, new Identifier("1"));
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
			ctor = Call.binaryOp(half, Operator.PLUS, half);
			if(odd) ctor = Call.binaryOp(ctor, Operator.PLUS, new Identifier("1"));
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
			ctor = Call.binaryOp(half, Operator.PLUS, half);
			if(odd) ctor = Call.binaryOp(ctor, Operator.PLUS, new Identifier("1"));
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

}
