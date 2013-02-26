package banjo.parser.ast;

import banjo.parser.util.FileRange;

public class BinaryOp extends Expr {
	private final Expr left;
	private final Expr right;
	private final Operator operator;
	
	public BinaryOp(Operator op, Expr left, Expr right) {
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
	public Operator getOp() {
		return operator;
	}

	@Override
	public int getStartColumn() {
		return left.getStartColumn();
	}
	
	
}
