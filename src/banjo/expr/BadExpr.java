package banjo.expr;

/**
 * Marker interface for expressions that serve as placeholder for
 * bad input expressions.
 */
public interface BadExpr extends Expr {

	String getMessage();
}
