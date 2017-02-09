package banjo.expr.core;

import banjo.expr.source.Operator;
import banjo.expr.source.Precedence;
import banjo.expr.util.OrdUtil;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.data.Set;

public class Extend extends AbstractCoreExpr implements CoreExpr {
	protected static final Ord<Extend> extendOrd = OrdUtil.chain(
			CoreExprOrd.ORD.contramap((e) -> e.base),
			CoreExprOrd.ORD.contramap((e) -> e.extension)
	);
	public final CoreExpr base;
	public final CoreExpr extension;

	public Extend(Set<SourceFileRange> ranges, CoreExpr base, CoreExpr extension) {
		super(base.hashCode() ^ extension.hashCode(), ranges);
		this.base = base;
		this.extension = extension;
	}

	public Extend(CoreExpr base, CoreExpr extension) {
		this(SourceFileRange.EMPTY_SET, base, extension);
	}

	public Extend(int hashCode, Set<SourceFileRange> ranges, CoreExpr base, CoreExpr extension) {
		super(hashCode+ranges.hashCode()+base.hashCode()+extension.hashCode(), ranges);
		this.base = base;
		this.extension = extension;
	}


	@Override
	public void toSource(StringBuffer sb) {
		this.base.toSource(sb, Operator.EXTENSION.getPrecedence());
		sb.append(' ');
		sb.append(Operator.EXTENSION.getOp());
		sb.append(' ');
		this.extension.toSource(sb, Operator.EXTENSION.getPrecedence());
	}

	@Override
	public String toString() {
		return base.toString() + " " + Operator.EXTENSION.getOp() + " " + extension.toString();
	}

	@Override
	public Precedence getPrecedence() {
		return Operator.EXTENSION.getPrecedence();
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
	public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
		return visitor.extend(getRanges(), base.acceptVisitor(visitor), extension.acceptVisitor(visitor));
	}


}
