package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.AbstractBadExpr;
import banjo.dom.BadExpr;

public class BadCoreExpr extends AbstractBadExpr implements CoreExpr, BadExpr {
	public static final class UnknownType extends BadCoreExpr {
		public UnknownType() {
			super("Unable to determine type");
		}
	}

	public BadCoreExpr(String message) {
		super(message);
	}

	@Override
	@Nullable
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.badExpr(this);
	}
}
