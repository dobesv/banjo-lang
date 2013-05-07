package banjo.dom.token;

import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.core.CoreExpr;
import banjo.dom.core.CoreExprVisitor;
import banjo.dom.core.BaseCoreExprVisitor;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExprVisitor;


public class NumberLiteral extends AbstractAtom implements Atom {
	final String text;
	final Number number;

	public NumberLiteral(int sourceLength, String text, Number number) {
		super(sourceLength, number.hashCode());
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
}
