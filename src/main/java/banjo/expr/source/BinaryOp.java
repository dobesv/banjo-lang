package banjo.expr.source;

import banjo.expr.BadExpr;
import banjo.expr.util.OrdUtil;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.data.List;
import fj.data.Set;

public class BinaryOp extends AbstractOp implements SourceExpr {
	public static final Ord<BinaryOp> _leftRightOrd = OrdUtil.chain(
			sourceExprOrd.contramap((BinaryOp x) -> x.left),
			sourceExprOrd.contramap((BinaryOp x) -> x.right)
	);
	public static final Ord<BinaryOp> ORD = OrdUtil.chain(
			AbstractOp.<BinaryOp>_operatorOrd(),
			_leftRightOrd
	);
	private final SourceExpr left;
	private final SourceExpr right;

	public BinaryOp(Set<SourceFileRange> ranges, Operator operator, Set<SourceFileRange> operatorRanges, SourceExpr left, SourceExpr right) {
		super(ranges, operator, operatorRanges, left, right);
		this.left = left;
		this.right = right;
	}

	public BinaryOp(Operator operator, Set<SourceFileRange> operatorRanges, SourceExpr left, SourceExpr right) {
		this(left.getRanges().union(right.getRanges()).union(operatorRanges),
				operator, operatorRanges, left, right);
	}
	public BinaryOp(Operator operator, SourceExpr left, SourceExpr right) {
		this(left.getRanges().union(right.getRanges()),
				operator, SourceFileRange.EMPTY_SET, left, right);
	}
	public SourceExpr getLeft() {
		return this.left;
	}
	public SourceExpr getRight() {
		return this.right;
	}

	@Override
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.binaryOp(this);
	}
	@Override
	public <T> T acceptVisitor(SourceExprAlgebra<T> visitor) {
		return visitor.binaryOp(getRanges(), getOperator(), getOperatorRanges(), getLeft().acceptVisitor(visitor), getRight().acceptVisitor(visitor));
	}


	@Override
	public void toSource(StringBuffer sb) {
		this.left.toSource(sb, getPrecedence());
		if(this.operator.isParen()) {
			sb.append(this.operator.getParenType().getStartChar());
			this.right.toSource(sb, Precedence.lowest());
			sb.append(this.operator.getParenType().getEndChar());
		} else {
            switch (this.operator) {
            case COMMA:
            case PROJECTION:
            case POW:
                break;

            default:
                sb.append(' ');
            }
            sb.append(this.operator.getOp());
            switch (this.operator) {
            case PROJECTION:
            case POW:
                break;

            default:
                sb.append(' ');
            }
			this.right.toSource(sb, getPrecedence());
		}
	}

	@Override
	public void toFullyParenthesizedSource(StringBuffer sb) {
		if(this.left.getPrecedence() != Precedence.ATOM) sb.append('(');
		this.left.toFullyParenthesizedSource(sb);
		if(this.left.getPrecedence() != Precedence.ATOM) sb.append(')');
		if(this.operator.isParen()) {
			sb.append(this.operator.getParenType().getStartChar());
			this.right.toFullyParenthesizedSource(sb);
			sb.append(this.operator.getParenType().getEndChar());
		} else {
			sb.append(' ');
			this.operator.toSource(sb);
			sb.append(' ');
			if(this.right.getPrecedence() != Precedence.ATOM) sb.append('(');
			this.right.toFullyParenthesizedSource(sb);
			if(this.right.getPrecedence() != Precedence.ATOM) sb.append(')');
		}
	}

	@Override
	public List<BadExpr> getProblems() {
		return this.left.getProblems().append(this.right.getProblems());
	}

    public static <T extends SourceExpr> SourceExpr insertOperator(Operator operator, List<T> operands) {
        return operands.foldRight(
                (T left, SourceExpr right) -> (right.isEmpty() ? left : new BinaryOp(operator, left, right)),
                EmptyExpr.SYNTHETIC_INSTANCE);
    }

    public static <T extends SourceExpr> SourceExpr insertCommas(List<T> operands) {
        return insertOperator(Operator.COMMA, operands);
    }
}
