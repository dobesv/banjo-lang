package banjo.expr.source;

import banjo.expr.BadExpr;
import banjo.expr.util.OrdUtil;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.data.List;

public class UnaryOp extends AbstractOp implements SourceExpr {
	public static final Ord<UnaryOp> ORD = OrdUtil.chain(
			Operator.ORD.contramap(x -> x.operator),
			sourceExprOrd.contramap(x -> x.operand)
	);
	private final SourceExpr operand;

	public UnaryOp(List<SourceFileRange> ranges, Operator operator, List<SourceFileRange> operatorRanges, SourceExpr operand) {
		super(ranges, operator, operatorRanges, operand);
		this.operand = operand;
	}
    public UnaryOp(Operator operator, List<SourceFileRange> operatorRanges, SourceExpr operand) {
    	this(operatorRanges.append(operand.getSourceFileRanges()).sort(SourceFileRange.ORD),
    			operator, operatorRanges, operand);
    }
	public SourceExpr getOperand() {
		return this.operand;
	}
	@Override
	public Precedence getPrecedence() {
		return this.operator.getPrecedence();
	}

	@Override
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.unaryOp(this);
	}
	@Override
	public <T> T acceptVisitor(SourceExprAlgebra<T> visitor) {
		return visitor.unaryOp(getSourceFileRanges(), getOperator(), getOperatorRanges(), getOperand().acceptVisitor(visitor));
	}
	@Override
	public void toSource(StringBuffer sb) {
		final boolean paren = this.operator.isParen();
		final boolean prefix = this.operator.isPrefix();
		if(paren) sb.append(this.operator.getParenType().getStartChar());
		else if(prefix) sb.append(this.operator.getOp());
		this.operand.toSource(sb, paren?Precedence.lowest():getPrecedence());
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

	@Override
	public List<BadExpr> getProblems() {
		return this.operand.getProblems();
	}
}
