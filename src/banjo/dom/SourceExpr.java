package banjo.dom;

import org.eclipse.jdt.annotation.Nullable;

/**
 * An expression as it appeared in the source code, without any desugaring applied.
 */
public interface SourceExpr extends Expr {

	@Nullable <T> T acceptVisitor(SourceExprVisitor<T> visitor);
}
