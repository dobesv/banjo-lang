package banjo.dom.source;

import org.eclipse.jdt.annotation.Nullable;

public class BinaryOp extends AbstractOp implements SourceExpr {
	private final SourceExpr left;
	private final SourceExpr right;

	public BinaryOp(Operator operator, SourceExpr left, SourceExpr right) {
		super(operator, left, right);
		this.left = left;
		this.right = right;
	}

	public SourceExpr getLeft() {
		return this.left;
	}
	public SourceExpr getRight() {
		return this.right;
	}

	@Override @Nullable
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.binaryOp(this);
	}

	@Override
	public void toSource(StringBuffer sb) {
		this.left.toSource(sb, getPrecedence());
		sb.append(this.operator.getOp());
		this.right.toSource(sb, getPrecedence());
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
}
