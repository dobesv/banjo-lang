package banjo.dom.token;

import static banjo.parser.util.Check.nonNull;

import java.math.BigDecimal;
import java.math.BigInteger;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.BadExpr;
import banjo.dom.Expr;
import banjo.dom.core.BaseCoreExprVisitor;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.CoreExprAlgebra;
import banjo.dom.core.CoreExprVisitor;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExprAlgebra;
import banjo.dom.source.SourceExprVisitor;
import banjo.parser.util.SourceFileRange;
import banjo.util.SourceNumber;
import fj.data.List;


public class NumberLiteral extends AbstractAtom implements Atom, Key {
	final Number number;

	public NumberLiteral(SourceFileRange sfr, Number number) {
		this(List.single(sfr), number);
	}
	public NumberLiteral(List<SourceFileRange> ranges, Number number) {
		super(number.hashCode(), ranges);
		this.number = number;
	}

	public Number getNumber() {
		return this.number;
	}

	@Override
	public String toString() {
		return nonNull(this.number.toString());
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
	public int compareTo(@Nullable Expr o) {
		if(this == o)
			return 0;
		if(o == null) return -1;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final NumberLiteral other = (NumberLiteral) o;
			if(cmp == 0) cmp = number.getClass().getName().compareTo(other.number.getClass().getName());
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
		return visitor.numberLiteral(getSourceFileRanges(), number);
	}

	@Override
	public <T> T acceptVisitor(SourceExprAlgebra<T> visitor) {
		return visitor.numberLiteral(getSourceFileRanges(), number);
	}

	@Override
	public List<String> getParts() {
		return List.single(number.toString());
	}

}
