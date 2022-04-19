package banjo.expr.core;

import banjo.expr.source.BaseSourceExprVisitor;
import banjo.expr.source.BinaryOp;
import banjo.expr.source.Operator;
import banjo.expr.source.Precedence;
import banjo.expr.source.SourceExpr;
import banjo.expr.source.SourceExprVisitor;
import banjo.expr.source.UnaryOp;
import banjo.expr.token.Identifier;
import banjo.expr.util.OrdUtil;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.data.List;
import fj.data.Set;

public class Extend extends AbstractCoreExpr implements CoreExpr {
	protected static final Ord<Extend> extendOrd = OrdUtil.chain(CoreExprOrd.ORD.contramap((e) -> e.base),
			CoreExprOrd.ORD.contramap((e) -> e.extension));
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
		super(hashCode + ranges.hashCode() + base.hashCode() + extension.hashCode(), ranges);
		this.base = base;
		this.extension = extension;
	}

	@Override
	public Precedence getPrecedence() {
		return Operator.OBJECT_COMPOSITION.getPrecedence();
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

	public static Object functionLiteral(Set<SourceFileRange> ranges, List<Identifier> args, CoreExpr body,
			List<Identifier> slotArgs) {
		return null;
	}

	public static CoreExpr composeSlots(List<CoreExpr> slots) {
		return slots.foldLeft(
				(CoreExpr result, CoreExpr s) -> result == Nil.SYNTHETIC_INSTANCE ? s : new Extend(result, s),
				Nil.SYNTHETIC_INSTANCE);
	}

	public static CoreExpr extension(CoreExpr a, CoreExpr b) {
		if (Nil.isNil(a))
			return b;
		if (Nil.isNil(b))
			return a;
		return new Extend(a, b);
	}
}
