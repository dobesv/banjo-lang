package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Expr;
import banjo.dom.source.Operator;
import banjo.dom.source.Precedence;

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

	public Inspect(CoreExpr target) {
		super(target.hashCode());
		this.target = target;
	}

	@Override
	public void toSource(StringBuffer sb) {
		this.target.toSource(sb, Operator.INSPECT.getPrecedence());
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
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final Inspect other = (Inspect) o;
			if(cmp == 0) cmp = this.target.compareTo(other.target);
		}
		return cmp;
	}

	@Override
	@Nullable
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.inspect(this);
	}

	@Override
	public boolean equals(@Nullable Object obj) {
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


}
