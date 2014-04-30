package banjo.dom.source;

import java.util.Arrays;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.AbstractExpr;
import banjo.dom.Expr;
import banjo.parser.util.SourceFileRange;

public abstract class AbstractOp extends AbstractExpr {
	protected final Operator operator;
	protected final SourceExpr[] operands;

	public AbstractOp(SourceFileRange sfr, Operator operator, SourceExpr ... operands) {
		super(operator.hashCode() ^ Arrays.hashCode(operands) + sfr.hashCode(), sfr);
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
	public int compareTo(@Nullable Expr o) {
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
			if(cmp == 0) cmp = super.compareTo(other);
		}
		return cmp;
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if(obj == this) return true;
		if(obj == null) return false;
		if(!(obj instanceof AbstractOp)) return false;
		if(obj.hashCode() != this.hashCode()) return false;
		final AbstractOp x = (AbstractOp) obj;
		return x.operator == this.operator && Arrays.equals(x.operands, this.operands);
	}

}
