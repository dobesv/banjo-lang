package banjo.parser.ast;

import banjo.parser.util.FileRange;

public class UnaryOp extends Expr {
	private final UnaryOperator operator;
	private final Expr operand;
	public UnaryOp(FileRange range, UnaryOperator operator, Expr operand) {
		super(range);
		this.operator = operator;
		this.operand = operand;
	}
	public UnaryOperator getOperator() {
		return operator;
	}
	public Expr getOperand() {
		return operand;
	}
	public Expr withNewOperand(Expr enriched) {
		if(enriched == this.operand)
			return this;
		return new UnaryOp(getFileRange(), operator, enriched);
	}
	
	public void toSource(StringBuffer sb) {
		sb.append(operator.getOp());
		sb.append(' ');
		operand.toSource(sb, getPrecedence());
	}
	
	@Override
	public Precedence getPrecedence() {
		return operator.getPrecedence();
	}
}
