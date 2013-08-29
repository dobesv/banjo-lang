package banjo.dom.source;

import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

public class BinaryOp extends AbstractOp implements SourceExpr {
	private final SourceExpr left;
	private final SourceExpr right;

	public BinaryOp(List<SourceNode> children, Operator operator, SourceExpr left, SourceExpr right) {
		super(children, operator, left, right);
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
}
