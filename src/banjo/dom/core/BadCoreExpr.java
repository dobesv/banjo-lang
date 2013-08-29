package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.AbstractBadExpr;
import banjo.dom.BadExpr;

public class BadCoreExpr extends AbstractBadExpr implements CoreExpr, BadExpr {

	public BadCoreExpr(String message) {
		super(message);
	}

	@Override
	@Nullable
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.badExpr(this);
	}
}
