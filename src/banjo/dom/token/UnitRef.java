package banjo.dom.token;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.ParenType;
import banjo.dom.source.AbstractSourceNode;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExpr;
import banjo.dom.source.SourceExprVisitor;

public class UnitRef extends AbstractSourceNode implements SourceExpr, Token {

	private final ParenType parenType;

	public UnitRef(int length) {
		super(length);
		this.parenType = ParenType.BRACES;
	}

	public UnitRef(int length, ParenType parenType) {
		super(length);
		this.parenType = parenType;
	}

	public ParenType getParenType() {
		return this.parenType;
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.appendCodePoint(this.parenType.getStartChar()).appendCodePoint(this.parenType.getEndChar());
	}

	@Override
	public @Nullable <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.visitUnit(this);
	}
}
