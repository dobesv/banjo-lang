package banjo.dom.source;

import java.util.Arrays;
import java.util.List;

import banjo.dom.AbstractExpr;
import banjo.dom.Expr;

public abstract class AbstractOp extends AbstractExpr {
	protected final Operator operator;
	protected final SourceExpr[] operands;

	public AbstractOp(List<SourceNode> children, Operator operator, SourceExpr ... operands) {
		super(operator.hashCode() ^ Arrays.hashCode(operands));
		this.operator = operator;
		this.operands = operands;
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
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final AbstractOp other = (AbstractOp) o;
			cmp = this.operator.compareTo(other.operator);
			for(int i=0; cmp == 0 && i < this.operands.length && i < other.operands.length; i++) {
				cmp = this.operands[i].compareTo(other.operands[i]);
			}
			if(cmp == 0) cmp = Integer.compare(this.operands.length, other.operands.length);
		}
		return cmp;
	}


}
