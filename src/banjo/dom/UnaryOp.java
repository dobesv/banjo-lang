package banjo.dom;

import banjo.parser.util.FileRange;

public class UnaryOp extends AbstractExpr {
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
		if(operator.isParen()) {
			sb.append(operator.getParenType().getStartChar());
		} if(!operator.isSuffix()) {
			sb.append(operator.getOp());
			if(operator.getPrecedence() != Precedence.UNARY_PREFIX || operand.getPrecedence() == Precedence.UNARY_PREFIX)
				sb.append(' '); // Put a space for bullets
		}
		operand.toSource(sb, getPrecedence());
		if(operator.isParen()) {
			sb.append(operator.getParenType().getEndChar());
		} else if(operator.isSuffix()) {
			sb.append(operator.getOp());
		}
	}
	
	@Override
	public Precedence getPrecedence() {
		return operator.getPrecedence();
	}
	
	@Override
	public Expr transform(ExprTransformer transformer) {
		Expr newOperand = transformer.transform(operand);
		FileRange newRange = transformer.transform(fileRange);
		if(newOperand == operand && newRange == fileRange)
			return this;
		return new UnaryOp(newRange, operator, newOperand);
	}
}
