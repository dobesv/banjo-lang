package banjo.dom.source;

import org.eclipse.jdt.annotation.Nullable;

public class UnaryOp extends AbstractOp implements SourceExpr {
	private final SourceExpr operand;

	public UnaryOp(Operator operator, SourceExpr operand) {
		super(operator, operand);
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
	@Override
	public void toSource(StringBuffer sb) {
		final boolean paren = this.operator.isParen();
		final boolean prefix = this.operator.isPrefix();
		if(paren) sb.append(this.operator.getParenType().getStartChar());
		else if(prefix) sb.append(this.operator.getOp());
		this.operand.toSource(sb, getPrecedence());
		if(paren) sb.append(this.operator.getParenType().getEndChar());
		else if(!prefix) sb.append(this.operator.getOp());
	}

	@Override
	public void toFullyParenthesizedSource(StringBuffer sb) {
		final boolean paren = this.operator.isParen();
		final boolean prefix = this.operator.isPrefix();
		if(paren) sb.append(this.operator.getParenType().getStartChar());
		else {
			if(prefix) sb.append(this.operator.getOp());
			if(this.operand.getPrecedence() != Precedence.ATOM) sb.append('(');
		}
		this.operand.toFullyParenthesizedSource(sb);
		if(paren) sb.append(this.operator.getParenType().getEndChar());
		else {
			if(this.operand.getPrecedence() != Precedence.ATOM) sb.append(')');
			if(!prefix) sb.append(this.operator.getOp());
		}
	}

}
