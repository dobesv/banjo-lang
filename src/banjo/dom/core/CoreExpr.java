package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Expr;
import banjo.dom.source.SourceExpr;

/**
 * Core expressions are the ones that may be emitted by the desugaring process.
 */
public interface CoreExpr extends Expr {

	/**
	 * Get the expression that was desugared to produce this one.  The result should
	 * be used only to locate the expression in the source code for error reporting
	 * or change detection purposes; because information is lost in the desugaring
	 * process, the result may be shared by multiple CoreExpr instances, and it might
	 * not map to the same CoreExpr you are looking at if you were to desugar it again.
	 */
	SourceExpr getSourceExpr();

	@Nullable
	<T> T acceptVisitor(CoreExprVisitor<T> visitor);
}
