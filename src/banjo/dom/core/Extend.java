package banjo.dom.core;

import banjo.dom.Expr;
import banjo.dom.source.Operator;
import banjo.dom.source.Precedence;
import banjo.parser.util.SourceFileRange;
import fj.Ord;
import fj.data.List;

public class Extend extends AbstractCoreExpr implements CoreExpr {
	protected static final Ord<Extend> extendOrd = Ord.chain(
			CoreExpr.coreExprOrd.comap((e) -> e.base),
			CoreExpr.coreExprOrd.comap((e) -> e.extension)
	);
	private final CoreExpr base;
	private final CoreExpr extension;

	public Extend(List<SourceFileRange> ranges, CoreExpr base, CoreExpr extension) {
		super(base.hashCode() ^ extension.hashCode(), ranges);
		this.base = base;
		this.extension = extension;
	}

	public Extend(CoreExpr base, CoreExpr extension) {
		this(SourceFileRange.EMPTY_LIST, base, extension);
	}

	public Extend(int hashCode, List<SourceFileRange> ranges, CoreExpr base, CoreExpr extension) {
		super(hashCode+ranges.hashCode()+base.hashCode()+extension.hashCode(), ranges);
		this.base = base;
		this.extension = extension;
	}


	@Override
	public void toSource(StringBuffer sb) {
		this.base.toSource(sb, Operator.EXTEND.getPrecedence());
		sb.append(' ');
		sb.append(Operator.EXTEND.getOp());
		sb.append(' ');
		this.extension.toSource(sb, Operator.EXTEND.getPrecedence());
	}

	@Override
	public String toString() {
		return base.toString() + " " + Operator.EXTEND.getOp() + " " + extension.toString();
	}

	@Override
	public Precedence getPrecedence() {
		return Operator.EXTEND.getPrecedence();
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
		return visitor.extend(getSourceFileRanges(), base.acceptVisitor(visitor), extension.acceptVisitor(visitor));
	}


}
