package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Expr;
import banjo.dom.source.Operator;
import banjo.dom.source.Precedence;

public class Extend extends AbstractCoreExpr implements CoreExpr {
	private final CoreExpr base;
	private final CoreExpr extension;

	public Extend(CoreExpr base, CoreExpr extension) {
		super(base.hashCode() ^ extension.hashCode());
		this.base = base;
		this.extension = extension;
	}


	public Extend(int hashCode, CoreExpr base, CoreExpr extension) {
		super(hashCode);
		this.base = base;
		this.extension = extension;
	}


	@Override
	public void toSource(StringBuffer sb) {
		this.base.toSource(sb, Operator.EXTEND.getPrecedence());
		sb.append(Operator.EXTEND.getOp());
		this.extension.toSource(sb, Operator.EXTEND.getPrecedence());
	}

	@Override
	public Precedence getPrecedence() {
		return Operator.EXTEND.getPrecedence();
	}

	@Override
	public int compareTo(Expr o) {
		if(this == o)
			return 0;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final Extend other = (Extend) o;
			if(cmp == 0) cmp = this.base.compareTo(other.base);
			if(cmp == 0) cmp = this.extension.compareTo(other.extension);
		}
		return cmp;
	}

	@Override
	@Nullable
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.extend(this);
	}


	public CoreExpr getBase() {
		return this.base;
	}


	public CoreExpr getExtension() {
		return this.extension;
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (!(obj instanceof Extend))
			return false;
		final Extend other = (Extend) obj;
		if (!this.base.equals(other.base))
			return false;
		if (!this.extension.equals(other.extension))
			return false;
		return true;
	}


}
