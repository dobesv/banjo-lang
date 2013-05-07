package banjo.dom.source;

import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

public class UnaryOp extends AbstractOp implements SourceExpr {
	private final SourceExpr operand;

	public UnaryOp(List<SourceNode> children, Operator operator, SourceExpr operand) {
		super(children, operator);
		this.operand = operand;
	}
	public SourceExpr getOperand() {
		return this.operand;
	}
	@Override
	public Precedence getPrecedence() {
		return this.operator.getPrecedence();
	}

	@Override
	public @Nullable <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.unaryOp(this);
	}
}
