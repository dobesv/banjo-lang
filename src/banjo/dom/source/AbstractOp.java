package banjo.dom.source;

import java.util.Arrays;

import banjo.dom.AbstractExpr;
import banjo.dom.Expr;
import banjo.parser.util.ListUtil;
import banjo.parser.util.SourceFileRange;
import fj.data.List;

public abstract class AbstractOp extends AbstractExpr {
	public final Operator operator;
	public final SourceExpr[] operands;
	public final List<SourceFileRange> operatorRanges;

	public AbstractOp(List<SourceFileRange> ranges, Operator operator, List<SourceFileRange> operatorRanges, SourceExpr ... operands) {
		super(operator.hashCode() ^ Arrays.hashCode(operands) + ranges.hashCode(), ranges);
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

	@Override
	public int compareTo(Expr o) {
		if(this == o)
			return 0;
		if(o == null) return -1;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final AbstractOp other = (AbstractOp) o;
			cmp = this.operator.compareTo(other.operator);
			for(int i=0; cmp == 0 && i < this.operands.length && i < other.operands.length; i++) {
				cmp = this.operands[i].compareTo(other.operands[i]);
			}
			if(cmp == 0) cmp = Integer.compare(this.operands.length, other.operands.length);
			if(cmp == 0) cmp = ListUtil.compare(this.operatorRanges, other.operatorRanges);
			if(cmp == 0) cmp = super.compareTo(other);
		}
		return cmp;
	}

	@Override
	public boolean equals(Object obj) {
		if(obj == this) return true;
		if(obj == null) return false;
		if(!(obj instanceof AbstractOp)) return false;
		if(obj.hashCode() != this.hashCode()) return false;
		final AbstractOp x = (AbstractOp) obj;
		return x.operator == this.operator && Arrays.equals(x.operands, this.operands);
	}

	public List<SourceFileRange> getOperatorRanges() {
		return operatorRanges;
	}

}
