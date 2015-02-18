package banjo.dom.source;

import banjo.dom.BadExpr;
import banjo.parser.util.SourceFileRange;
import fj.data.List;

public class BinaryOp extends AbstractOp implements SourceExpr {
	private final SourceExpr left;
	private final SourceExpr right;

	public BinaryOp(List<SourceFileRange> ranges, Operator operator, List<SourceFileRange> operatorRanges, SourceExpr left, SourceExpr right) {
		super(ranges, operator, operatorRanges, left, right);
		this.left = left;
		this.right = right;
	}

	public BinaryOp(Operator operator, List<SourceFileRange> operatorRanges, SourceExpr left, SourceExpr right) {
		this(left.getSourceFileRanges().append(right.getSourceFileRanges()).append(operatorRanges).sort(SourceFileRange.ORD),
				operator, operatorRanges, left, right);
	}
	public BinaryOp(Operator operator, SourceExpr left, SourceExpr right) {
		this(left.getSourceFileRanges().append(right.getSourceFileRanges()).sort(SourceFileRange.ORD),
				operator, SourceFileRange.EMPTY_LIST, left, right);
	}
	public SourceExpr getLeft() {
		return this.left;
	}
	public SourceExpr getRight() {
		return this.right;
	}

	@Override
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.binaryOp(this);
	}
	@Override
	public <T> T acceptVisitor(SourceExprAlgebra<T> visitor) {
		return visitor.binaryOp(getSourceFileRanges(), getOperator(), getOperatorRanges(), getLeft().acceptVisitor(visitor), getRight().acceptVisitor(visitor));
	}


	@Override
	public void toSource(StringBuffer sb) {
		this.left.toSource(sb, getPrecedence());
		if(this.operator.isParen()) {
			sb.append(this.operator.getParenType().getStartChar());
			this.right.toSource(sb, Precedence.lowest());
			sb.append(this.operator.getParenType().getEndChar());
		} else {
			sb.append(' ');
			sb.append(this.operator.getOp());
			sb.append(' ');
			this.right.toSource(sb, getPrecedence());
		}
	}

	@Override
	public void toFullyParenthesizedSource(StringBuffer sb) {
		if(this.left.getPrecedence() != Precedence.ATOM) sb.append('(');
		this.left.toFullyParenthesizedSource(sb);
		if(this.left.getPrecedence() != Precedence.ATOM) sb.append(')');
		if(this.operator.isParen()) {
			sb.append(this.operator.getParenType().getStartChar());
			this.right.toFullyParenthesizedSource(sb);
			sb.append(this.operator.getParenType().getEndChar());
		} else {
			sb.append(' ');
			this.operator.toSource(sb);
			sb.append(' ');
			if(this.right.getPrecedence() != Precedence.ATOM) sb.append('(');
			this.right.toFullyParenthesizedSource(sb);
			if(this.right.getPrecedence() != Precedence.ATOM) sb.append(')');
		}
	}

	@Override
	public List<BadExpr> getProblems() {
		return this.left.getProblems().append(this.right.getProblems());
	}
}
