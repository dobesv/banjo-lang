package banjo.dom.source;

import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.token.OperatorRef;

public class BinaryOp extends AbstractOp implements SourceExpr {
	private final SourceExpr left;
	private final SourceExpr right;

	private static final @Nullable OperatorRef CLOSE_PAREN_NONE = null;
	public BinaryOp(List<SourceNode> children, Operator operator, SourceExpr left, SourceExpr right) {
		super(children, operator);
		this.left = left;
		this.right = right;
	}

	public BinaryOp(List<SourceNode> children, Operator operator, SourceExpr left, OperatorRef opToken, SourceExpr right) {
		this(children, operator, left, right);
	}

	public SourceExpr getLeft() {
		return this.left;
	}
	public SourceExpr getRight() {
		return this.right;
	}

	@Override @Nullable
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.visitBinaryOp(this);
	}
}
