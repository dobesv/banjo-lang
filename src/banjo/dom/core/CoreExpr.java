package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Expr;

/**
 * Core expressions are the ones that may be emitted by the desugaring process.
 */
public interface CoreExpr extends Expr {
	/**
	 * We are using the visitor pattern, not instanceof, to check the type of an expression.
	 */
	@Nullable
	<T> T acceptVisitor(CoreExprVisitor<T> visitor);
}
