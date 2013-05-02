package banjo.dom.token;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.core.CoreExprVisitor;
import banjo.dom.source.Atom;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExprVisitor;


public class NumberLiteral extends AbstractAtom implements Atom {
	final String text;
	final Number number;

	public NumberLiteral(int sourceLength, String text, Number number) {
		super(sourceLength);
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
		return visitor.visitNumberLiteral(this);
	}

	@Override
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.visitNumberLiteral(this);
	}

}
