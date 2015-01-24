package banjo.dom.core;

import banjo.dom.Expr;
import banjo.dom.source.Operator;
import banjo.dom.source.Precedence;
import banjo.parser.util.SourceFileRange;
import fj.data.List;

/**
 * A primitive operation for reading the metadata of an object.
 *
 * The metadata is a map from a method name to that method's metadata.
 */
public class Inspect extends AbstractCoreExpr implements CoreExpr {
	private final CoreExpr target;

	public CoreExpr getTarget() {
		return this.target;
	}

	public Inspect(List<SourceFileRange> ranges, CoreExpr target) {
		super(target.hashCode()+ranges.hashCode(), ranges);
		this.target = target;
	}

	@Override
	public void toSource(StringBuffer sb, String idPrefix) {
		this.target.toSource(sb, Operator.INSPECT.getPrecedence(), idPrefix);
		sb.append(Operator.INSPECT.getOp());
	}

	@Override
	public Precedence getPrecedence() {
		return Operator.INSPECT.getPrecedence();
	}

	@Override
	public int compareTo(Expr o) {
		if(this == o)
			return 0;
		if(o == null) return -1;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final Inspect other = (Inspect) o;
			if(cmp == 0) cmp = this.target.compareTo(other.target);
			if(cmp == 0) cmp = super.compareTo(o);
		}
		return cmp;
	}

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.inspect(this);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (!(obj instanceof Inspect))
			return false;
		final Inspect other = (Inspect) obj;
		if (!this.target.equals(other.target))
			return false;
		return true;
	}

	@Override
	public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
		return visitor.inspect(getSourceFileRanges(), target.acceptVisitor(visitor));
	}


}
