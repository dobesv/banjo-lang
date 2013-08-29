package banjo.dom.source;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.AbstractBadExpr;
import banjo.dom.BadExpr;
import banjo.dom.ParenType;

public class BadSourceExpr extends AbstractBadExpr implements SourceExpr, BadExpr {
	public static class ExpectedOperator extends BadSourceExpr {

		public ExpectedOperator() {
			super("Expected an operator here");
		}

	}
	public static class IncorrectIndentation extends BadSourceExpr {

		public IncorrectIndentation(int actualIndent, int expectedMinIndent) {
			super("Incorrect indentation; expected at least %d columns, got only %d", expectedMinIndent, actualIndent);
		}

	}
	public static class MismatchedCloseParen extends BadSourceExpr {
		private final ParenType closeParenType;

		public MismatchedCloseParen(ParenType closeParenType) {
			super("Mismatched closed paren %s", closeParenType.getEndChar());
			this.closeParenType = closeParenType;
		}

		public ParenType getCloseParenType() {
			return this.closeParenType;
		}
	}
	public static class UnsupportedOperator extends BadSourceExpr {
		private final String op;

		public UnsupportedOperator(String op) {
			super("Unsupported operator '"+op+"'");
			this.op = op;
		}

		public String getOp() {
			return this.op;
		}
	}

	protected BadSourceExpr(String message, Object ... args) {
		super(message, args);
	}

	@Override
	@Nullable
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.badSourceExpr(this);
	}

}
