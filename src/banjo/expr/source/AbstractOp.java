package banjo.expr.source;

import banjo.expr.AbstractExpr;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.data.List;

public abstract class AbstractOp extends AbstractExpr {
	protected static final <T extends AbstractOp> Ord<T> _operatorOrd() { return Operator.ORD.comap((x) -> x.operator); }
			
	public final Operator operator;
	public final SourceExpr[] operands;
	public final List<SourceFileRange> operatorRanges;

	public AbstractOp(List<SourceFileRange> ranges, Operator operator, List<SourceFileRange> operatorRanges, SourceExpr ... operands) {
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

	public List<SourceFileRange> getOperatorRanges() {
		return operatorRanges;
	}

}
