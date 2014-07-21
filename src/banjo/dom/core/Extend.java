package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import fj.data.List;
import banjo.dom.Expr;
import banjo.dom.source.Operator;
import banjo.dom.source.Precedence;
import banjo.parser.util.SourceFileRange;

public class Extend extends AbstractCoreExpr implements CoreExpr {
	private final CoreExpr base;
	private final CoreExpr extension;

	public Extend(List<SourceFileRange> ranges, CoreExpr base, CoreExpr extension) {
		super(base.hashCode() ^ extension.hashCode(), ranges);
		this.base = base;
		this.extension = extension;
	}


	public Extend(int hashCode, List<SourceFileRange> ranges, CoreExpr base, CoreExpr extension) {
		super(hashCode+ranges.hashCode(), ranges);
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
	public int compareTo(@Nullable Expr o) {
		if(this == o)
			return 0;
		if(o == null) return -1;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final Extend other = (Extend) o;
			if(cmp == 0) cmp = this.base.compareTo(other.base);
			if(cmp == 0) cmp = this.extension.compareTo(other.extension);
			if(cmp == 0) cmp = super.compareTo(o);
		}
		return cmp;
	}

	@Override
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


	@Override
	public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
		return visitor.extend(getSourceFileRanges(), base.acceptVisitor(visitor), extension.acceptVisitor(visitor));
	}


}
