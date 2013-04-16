package banjo.dom;

import org.eclipse.jdt.annotation.Nullable;

/**
 * Core expressions are the ones that may be emitted by the desugaring process.
 */
public interface CoreExpr extends Expr {

	@Nullable
	<T> T acceptVisitor(CoreExprVisitor<T> visitor);
}
