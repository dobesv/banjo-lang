package banjo.dom;

import org.eclipse.jdt.annotation.Nullable;

import banjo.parser.util.FileRange;
import fj.data.Option;

public class BinaryOp extends AbstractOp implements SourceExpr {
	private final SourceExpr left;
	private final SourceExpr right;
	
	private static final Option<OperatorRef> CLOSE_PAREN_NONE = Option.none();
	public BinaryOp(Operator operator, SourceExpr left, OperatorRef opToken, SourceExpr right, Option<OperatorRef> closeParenToken) {
		super(new FileRange(left.getFileRange(), closeParenToken.isNone() ? right.getFileRange() : closeParenToken.some().getFileRange()), operator, opToken, closeParenToken);
		this.left = left;
		this.right = right;
	}
	
	public BinaryOp(Operator operator, SourceExpr left, OperatorRef opToken, SourceExpr right) {
		this(operator, left, opToken, right, CLOSE_PAREN_NONE);
	}
	
	public SourceExpr getLeft() {
		return left;
	}
	public SourceExpr getRight() {
		return right;
	}

	@Override
	public int getStartColumn() {
		return left.getStartColumn();
	}
	
	@Override
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
	public Expr transform(ExprTransformer transformer) {
		Expr newLeft = transformer.transform(left);
		OperatorRef newOpToken = transformer.transform(opToken);
		Expr newRight = transformer.transform(right);
		Option<OperatorRef> newCloseParenToken = optTransform(closeParenToken, transformer);
		if(newLeft == left && 
				newOpToken == opToken && 
				newRight == right && 
				newCloseParenToken == this.closeParenToken)
			return this;
		return new BinaryOp(operator, left, opToken, right, newCloseParenToken);
	}

	@Override @Nullable
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.visitBinaryOp(this);
	}
}
