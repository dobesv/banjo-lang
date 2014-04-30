package banjo.dom.source;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.AbstractBadExpr;
import banjo.dom.BadExpr;
import banjo.dom.ParenType;
import banjo.parser.util.SourceFileRange;
import fj.data.List;

public class BadSourceExpr extends AbstractBadExpr implements SourceExpr, BadExpr {
	public static class ExpectedOperator extends BadSourceExpr {

		public ExpectedOperator(SourceFileRange sfr) {
			super(sfr, "Expected an operator here");
		}

	}
	public static class IncorrectIndentation extends BadSourceExpr {

		public IncorrectIndentation(SourceFileRange sfr, int actualIndent, int expectedMinIndent) {
			super(sfr, "Incorrect indentation; expected at least %d columns, got only %d", expectedMinIndent, actualIndent);
		}

	}
	public static class MismatchedCloseParen extends BadSourceExpr {
		private final ParenType closeParenType;

		public MismatchedCloseParen(SourceFileRange sfr, ParenType closeParenType) {
			super(sfr, "Mismatched close paren %s", closeParenType.getEndChar());
			this.closeParenType = closeParenType;
		}

		public ParenType getCloseParenType() {
			return this.closeParenType;
		}
	}
	public static class MissingCloseParen extends BadSourceExpr {
		private final ParenType closeParenType;

		public MissingCloseParen(SourceFileRange sfr, ParenType closeParenType) {
			super(sfr, "Missing close paren %s", closeParenType.getEndChar());
			this.closeParenType = closeParenType;
		}

		public ParenType getCloseParenType() {
			return this.closeParenType;
		}
	}
	public static class UnsupportedOperator extends BadSourceExpr {
		private final String op;

		public UnsupportedOperator(SourceFileRange sfr, String op) {
			super(sfr, "Unsupported operator '"+op+"'");
			this.op = op;
		}

		public String getOp() {
			return this.op;
		}
	}

	protected BadSourceExpr(SourceFileRange sfr, String message, Object ... args) {
		super(sfr, message, args);
	}

	@Override
	@Nullable
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.badSourceExpr(this);
	}

	@Override
	public List<BadExpr> getProblems() {
		return List.<BadExpr>single(this);
	}

}
