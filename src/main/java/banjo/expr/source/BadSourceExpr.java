package banjo.expr.source;

import banjo.expr.AbstractBadExpr;
import banjo.expr.BadExpr;
import banjo.expr.ParenType;
import banjo.expr.util.SourceFileRange;
import fj.data.List;
import fj.data.Set;

public class BadSourceExpr extends AbstractBadExpr implements SourceExpr, BadExpr {
	public static class ExpectedOperator extends BadSourceExpr {

		public ExpectedOperator(Set<SourceFileRange> ranges) {
			super(ranges, "Expected an operator here");
		}

	}
	public static class IncorrectIndentation extends BadSourceExpr {

		public IncorrectIndentation(Set<SourceFileRange> ranges, int actualIndent, int expectedMinIndent) {
			super(ranges, "Incorrect indentation; expected at least %d columns, got only %d", expectedMinIndent, actualIndent);
		}

	}
	public static class MismatchedCloseParen extends BadSourceExpr {
		private final ParenType closeParenType;

		public MismatchedCloseParen(Set<SourceFileRange> ranges, ParenType openParenType, ParenType closeParenType) {
			super(ranges, "Mismatched close paren; expecting '%s' but got '%s'", openParenType.getEndChar(), closeParenType.getEndChar());
			this.closeParenType = closeParenType;
		}

		public MismatchedCloseParen(Set<SourceFileRange> ranges, ParenType closeParenType) {
			super(ranges, "Mismatched close paren '%s'; no matching open paren found", closeParenType.getEndChar());
			this.closeParenType = closeParenType;
		}

		public ParenType getCloseParenType() {
			return this.closeParenType;
		}
	}
	public static class MissingCloseParen extends BadSourceExpr {
		private final ParenType closeParenType;

		public MissingCloseParen(Set<SourceFileRange> ranges, ParenType closeParenType) {
			super(ranges, "Missing close paren %s", closeParenType.getEndChar());
			this.closeParenType = closeParenType;
		}

		public ParenType getCloseParenType() {
			return this.closeParenType;
		}
	}
	public static class UnsupportedOperator extends BadSourceExpr {
		private final String op;

		public UnsupportedOperator(Set<SourceFileRange> ranges, String op) {
			super(ranges, "Unsupported operator '"+op+"'");
			this.op = op;
		}

		public String getOp() {
			return this.op;
		}
	}

	public static class InvalidProjection extends BadSourceExpr {

		public InvalidProjection(Set<SourceFileRange> ranges, String message,
                Object... args) {
	        super(ranges, message, args);
        }

		public InvalidProjection(SourceFileRange sfr, String message) {
	        super(sfr, message);
        }

	}

	protected BadSourceExpr(Set<SourceFileRange> ranges, String message, Object ... args) {
		super(ranges, message, args);
	}

	public BadSourceExpr(SourceFileRange sfr, String message) {
		this(Set.single(SourceFileRange.ORD, sfr), message);
	}

	@Override
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.badSourceExpr(this);
	}

	@Override
	public <T> T acceptVisitor(SourceExprAlgebra<T> visitor) {
		return visitor.badExpr(getRanges(), getMessageTemplate(), getArgs());
	}

	@Override
	public List<BadExpr> getProblems() {
		return List.single(this);
	}

}
