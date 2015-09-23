package banjo.expr.source;

import banjo.expr.AbstractExpr;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.data.Set;

public abstract class AbstractOp extends AbstractExpr {
	protected static final <T extends AbstractOp> Ord<T> _operatorOrd() { return Operator.ORD.contramap((x) -> x.operator); }
			
	public final Operator operator;
	public final SourceExpr[] operands;
	public final Set<SourceFileRange> operatorRanges;

	public AbstractOp(Set<SourceFileRange> ranges, Operator operator, Set<SourceFileRange> operatorRanges, SourceExpr ... operands) {
		super(ranges);
		this.operator = operator;
		this.operands = operands;
		this.operatorRanges = operatorRanges;
	}

	public Operator getOperator() {
		return this.operator;
	}

	@Override
	public Precedence getPrecedence() {
		return this.operator.getPrecedence();
	}

	public Set<SourceFileRange> getOperatorRanges() {
		return operatorRanges;
	}

}
