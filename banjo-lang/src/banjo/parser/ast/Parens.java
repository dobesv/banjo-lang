package banjo.parser.ast;

import banjo.parser.util.FileRange;

/**
 * Represents an expression that was wrapped in parens "(" and ")" to ensure correct
 * operator precedence.
 */
public class Parens extends Expr {

	private final Expr expression;
	private final ParenType parenType;

	public Parens(FileRange range, Expr expr, ParenType parenType) {
		super(range);
		this.expression = expr;
		this.parenType = parenType;
	}

	public Expr getExpression() {
		return expression;
	}

	public ParenType getParenType() {
		return parenType;
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}
	
	@Override
	public void toSource(StringBuffer sb) {
		sb.appendCodePoint(parenType.getStartChar());
		expression.toSource(sb);
		sb.appendCodePoint(parenType.getEndChar());
	}
}
