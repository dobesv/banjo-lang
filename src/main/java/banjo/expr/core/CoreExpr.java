package banjo.expr.core;

import banjo.expr.Expr;
import banjo.expr.source.SourceExpr;
import fj.data.Set;

/**
 * Core expressions are the ones that may be emitted by the desugaring process.
 */
public interface CoreExpr extends Expr {


	/**
	 * Visitor pattern
	 */
    <T> T acceptVisitor(CoreExprVisitor<T> visitor);

	/**
	 * Object algebra pattern (kind of a bottom-up visitor pattern)
	 */
	<T> T acceptVisitor(CoreExprAlgebra<T> visitor);

    public static final Set<CoreExpr> EMPTY_SET = Set.empty(CoreExprOrd.ORD);

	/**
	 * Parse a string to a CoreExpr
	 */
	public static CoreExpr fromString(String src) {
		final SourceExpr parseTree = SourceExpr.fromString(src);
		return fromSourceExpr(parseTree);
	}

    /**
     * Parse a SourceExpr syntax tree into a CoreExpr AST.
     */
	public static CoreExpr fromSourceExpr(final SourceExpr parseTree) {
	    return new CoreExprFactory().desugar(parseTree).getValue();
    }

	/**
	 * True if this CoreExpr is the same as the other, ignoring source file location.
	 */
	default boolean eql(CoreExpr other) {
        return CoreExprOrd.eql(this, other);
	}

}
