package banjo.dom;

import banjo.parser.util.FileRange;

public class BinaryOp extends AbstractExpr implements ParseTreeNode {
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
		if(operator.isParen()) {
			sb.append(operator.getParenType().getStartChar());
		} else {
			sb.append(operator.getOp());
			sb.append(' ');
		}
		right.toSource(sb, getPrecedence());
		if(operator.isParen()) {
			sb.append(operator.getParenType().getEndChar());
		}
	}
	
	@Override
	public Precedence getPrecedence() {
		return operator.getPrecedence();
	}

	@Override
	public Expr transform(ExprTransformer transformer) {
		return with(transformer.transform(left), transformer.transform(right));
	}

	private Expr with(Expr left, Expr right) {
		if(left == this.left && right == this.right)
			return this;
		return new BinaryOp(operator, left, right);
	}
	
	@Override
	public <T> T acceptVisitor(ParseTreeVisitor<T> visitor) {
		return visitor.visitBinaryOp(this);
	}
}
