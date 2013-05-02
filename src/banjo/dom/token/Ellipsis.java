package banjo.dom.token;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.AbstractSourceNode;
import banjo.dom.source.SourceExpr;
import banjo.dom.source.SourceExprVisitor;

public class Ellipsis extends AbstractSourceNode implements SourceExpr, Token {

	public Ellipsis(int length) {
		super(length);
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append("...");
	}

	@Override
	public @Nullable <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.visitEllipsis(this);
	}
}
