package banjo.expr.core;

import banjo.expr.source.Operator;
import banjo.expr.source.Precedence;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.data.Set;

/**
 * A primitive operation for reading the metadata of an object.
 *
 * The metadata is a map from a method name to that method's metadata.
 */
public class Inspect extends AbstractCoreExpr implements CoreExpr {
	public static final Ord<Inspect> ORD = CoreExpr.coreExprOrd.contramap((x) -> x.target);
	public final CoreExpr target;

	public CoreExpr getTarget() {
		return this.target;
	}

	public Inspect(Set<SourceFileRange> ranges, CoreExpr target) {
		super(target.hashCode()+ranges.hashCode(), ranges);
		this.target = target;
	}

	@Override
	public void toSource(StringBuffer sb) {
		this.target.toSource(sb, Operator.INSPECT.getPrecedence());
		sb.append(Operator.INSPECT.getOp());
	}

	@Override
	public String toString() {
		return target.toString() + Operator.INSPECT.getOp();
	}

	@Override
	public Precedence getPrecedence() {
		return Operator.INSPECT.getPrecedence();
	}

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.inspect(this);
	}

	@Override
	public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
		return visitor.inspect(getSourceFileRanges(), target.acceptVisitor(visitor));
	}


}
