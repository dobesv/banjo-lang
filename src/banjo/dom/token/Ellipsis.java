package banjo.dom.token;

import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.AbstractSourceNode;
import banjo.dom.source.BaseSourceExprVisitor;
import banjo.dom.source.SourceExpr;
import banjo.dom.source.SourceExprVisitor;

public class Ellipsis extends AbstractSourceNode implements SourceExpr, Token {

	public Ellipsis(int length) {
		super(length, 31);
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append("...");
	}

	@Override
	public @Nullable <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.ellipsis(this);
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		return super.equals(obj) && (obj instanceof Ellipsis);
	}

	public static boolean isEllipsis(SourceExpr expr) {
		return nonNull(expr.acceptVisitor(new BaseSourceExprVisitor<Boolean>() {
			@Override
			@Nullable
			public Boolean ellipsis(Ellipsis ellipsis) {
				return true;
			}

			@Override
			@Nullable
			public Boolean fallback(SourceExpr other) {
				return false;
			}
		}));
	}
}
