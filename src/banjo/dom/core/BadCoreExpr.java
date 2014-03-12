package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.AbstractBadExpr;
import banjo.dom.BadExpr;
import banjo.parser.util.SourceFileRange;

public class BadCoreExpr extends AbstractBadExpr implements CoreExpr, BadExpr {
	public static final class UnknownType extends BadCoreExpr {
		public UnknownType(SourceFileRange sfr) {
			super(sfr, "Unable to determine type");
		}
	}

	public BadCoreExpr(SourceFileRange sfr, String message) {
		super(sfr, message);
	}

	@Override
	@Nullable
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.badExpr(this);
	}
}
