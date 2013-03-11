package banjo.parser.ast;

import banjo.parser.util.FileRange;

public class BinaryOp extends Expr {
	private final Expr left;
	private final Expr right;
	private final BinaryOperator operator;
	
	public BinaryOp(BinaryOperator op, Expr left, Expr right) {
		super(new FileRange(left.getFileRange(), right.getFileRange()));
		this.operator = op;
		this.left = left;
		this.right = right;
	}
	
	public Expr getLeft() {
		return left;
	}
	public Expr getRight() {
		return right;
	}
	public BinaryOperator getOperator() {
		return operator;
	}

	@Override
	public int getStartColumn() {
		return left.getStartColumn();
	}
	
	public void toSource(StringBuffer sb) {
		left.toSource(sb, getPrecedence());
		sb.append(' ');
		sb.append(operator.getOp());
		sb.append(' ');
		right.toSource(sb, getPrecedence());
	}
	
	@Override
	public Precedence getPrecedence() {
		return operator.getPrecedence();
	}

}
