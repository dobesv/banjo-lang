package banjo.parser.ast;

import banjo.parser.util.FileRange;

/**
 * Represents an expression that was wrapped in parens "(" and ")" to ensure correct
 * operator precedence.
 */
public class Parens extends Expr {

	private final Expr expression;

	public Parens(FileRange range, Expr expr) {
		super(range);
		this.expression = expr;
	}

	public Expr getExpression() {
		return expression;
	}

}
