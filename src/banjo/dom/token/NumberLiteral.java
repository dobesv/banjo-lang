package banjo.dom.token;

import static banjo.parser.util.Check.nonNull;

import java.math.BigDecimal;
import java.math.BigInteger;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Expr;
import banjo.dom.core.BaseCoreExprVisitor;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.CoreExprVisitor;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExprVisitor;
import banjo.parser.util.SourceFileRange;


public class NumberLiteral extends AbstractAtom implements Atom, Key {
	final String text;
	final Number number;

	public NumberLiteral(SourceFileRange sfr, String text, Number number) {
		super(number.hashCode(), sfr);
		this.text = text;
		this.number = number;
	}

	public Number getNumber() {
		return this.number;
	}

	@Override
	public String toString() {
		return this.text;
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append(this.text);
	}

	@Override
	public @Nullable <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.numberLiteral(this);
	}

	@Override
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.numberLiteral(this);
	}

	public static boolean isNumberLiteral(CoreExpr x) {
		return nonNull(x.acceptVisitor(new BaseCoreExprVisitor<Boolean>() {
			@Override
			@Nullable
			public Boolean numberLiteral(NumberLiteral n) {
				return true;
			}
			@Override
			@Nullable
			public Boolean fallback(CoreExpr unsupported) {
				return false;
			}
		})).booleanValue();
	}

	@Override
	public int compareTo(Expr o) {
		if(this == o)
			return 0;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final NumberLiteral other = (NumberLiteral) o;
			if(cmp == 0) cmp = this.text.compareTo(other.text); // TODO Comparing by text here because Number doesn't implement Comparable (!?!?)
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
	public String getKeyString() {
		return this.text;
	}
}
