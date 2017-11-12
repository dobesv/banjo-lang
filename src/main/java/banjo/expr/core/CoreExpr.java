package banjo.expr.core;

import banjo.expr.Expr;
import banjo.expr.source.Precedence;
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
        return new CoreExprFactory().desugar(parseTree);
    }

	/**
	 * True if this CoreExpr is the same as the other, ignoring source file location.
	 */
	default boolean eql(CoreExpr other) {
        return CoreExprOrd.eql(this, other);
	}

    @Override
    public default String toSource() {
        return toSourceExpr().toSource();
    }

    @Override
    public default String toSource(Precedence precedence) {
        return toSourceExpr().toSource(precedence);
    }

    @Override
    default void toSource(StringBuffer sb) {
        toSourceExpr().toSource(sb);
    }

    @Override
    public default void toSource(StringBuffer sb, Precedence precedence) {
        toSourceExpr().toSource(sb, precedence);
    }

    /**
     * Convert to a SourceExpr for code formatting / printing purposes.
     */
    public SourceExpr toSourceExpr();

}
