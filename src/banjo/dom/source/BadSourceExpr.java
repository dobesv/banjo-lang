package banjo.dom.source;

import banjo.dom.AbstractBadExpr;
import banjo.dom.BadExpr;
import banjo.dom.ParenType;
import banjo.parser.util.SourceFileRange;
import fj.data.List;

public class BadSourceExpr extends AbstractBadExpr implements SourceExpr, BadExpr {
	public static class ExpectedOperator extends BadSourceExpr {

		public ExpectedOperator(List<SourceFileRange> ranges) {
			super(ranges, "Expected an operator here");
		}

	}
	public static class IncorrectIndentation extends BadSourceExpr {

		public IncorrectIndentation(List<SourceFileRange> ranges, int actualIndent, int expectedMinIndent) {
			super(ranges, "Incorrect indentation; expected at least %d columns, got only %d", expectedMinIndent, actualIndent);
		}

	}
	public static class MismatchedCloseParen extends BadSourceExpr {
		private final ParenType closeParenType;

		public MismatchedCloseParen(List<SourceFileRange> ranges, ParenType openParenType, ParenType closeParenType) {
			super(ranges, "Mismatched close paren; expecting '%s' but got '%s'", openParenType.getEndChar(), closeParenType.getEndChar());
			this.closeParenType = closeParenType;
		}

		public MismatchedCloseParen(List<SourceFileRange> ranges, ParenType closeParenType) {
			super(ranges, "Mismatched close paren '%s'; no matching open paren found", closeParenType.getEndChar());
			this.closeParenType = closeParenType;
		}

		public ParenType getCloseParenType() {
			return this.closeParenType;
		}
	}
	public static class MissingCloseParen extends BadSourceExpr {
		private final ParenType closeParenType;

		public MissingCloseParen(List<SourceFileRange> ranges, ParenType closeParenType) {
			super(ranges, "Missing close paren %s", closeParenType.getEndChar());
			this.closeParenType = closeParenType;
		}

		public ParenType getCloseParenType() {
			return this.closeParenType;
		}
	}
	public static class UnsupportedOperator extends BadSourceExpr {
		private final String op;

		public UnsupportedOperator(List<SourceFileRange> ranges, String op) {
			super(ranges, "Unsupported operator '"+op+"'");
			this.op = op;
		}

		public String getOp() {
			return this.op;
		}
	}

	protected BadSourceExpr(List<SourceFileRange> ranges, String message, Object ... args) {
		super(ranges, message, args);
	}

	public BadSourceExpr(SourceFileRange sfr, String message) {
		this(List.single(sfr), message);
	}

	@Override
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.badSourceExpr(this);
	}

	@Override
	public <T> T acceptVisitor(SourceExprAlgebra<T> visitor) {
		return visitor.badExpr(getSourceFileRanges(), getMessageTemplate(), getArgs());
	}

	@Override
	public List<BadExpr> getProblems() {
		return List.single(this);
	}

}
