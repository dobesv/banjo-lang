package banjo.dom.source;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.BadExpr;
import banjo.parser.util.SourceFileRange;
import fj.data.List;

public class BinaryOp extends AbstractOp implements SourceExpr {
	private final SourceExpr left;
	private final SourceExpr right;

	public BinaryOp(List<SourceFileRange> ranges, Operator operator, SourceFileRange operatorRange, SourceExpr left, SourceExpr right) {
		super(ranges, operator, operatorRange, left, right);
		this.left = left;
		this.right = right;
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
		return visitor.binaryOp(getSourceFileRanges(), getOperator(), getOperatorRange(), getLeft().acceptVisitor(visitor), getRight().acceptVisitor(visitor));
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
		sb.append(' ');
		this.operator.toSource(sb);
		sb.append(' ');
		if(this.right.getPrecedence() != Precedence.ATOM) sb.append('(');
		this.right.toFullyParenthesizedSource(sb);
		if(this.right.getPrecedence() != Precedence.ATOM) sb.append(')');
	}

	@Override
	public List<BadExpr> getProblems() {
		return this.left.getProblems().append(this.right.getProblems());
	}
}
