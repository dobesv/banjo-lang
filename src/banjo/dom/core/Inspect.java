package banjo.dom.core;

import banjo.dom.source.Operator;
import banjo.dom.source.Precedence;
import banjo.parser.util.SourceFileRange;
import fj.Ord;
import fj.data.List;

/**
 * A primitive operation for reading the metadata of an object.
 *
 * The metadata is a map from a method name to that method's metadata.
 */
public class Inspect extends AbstractCoreExpr implements CoreExpr {
	public static final Ord<Inspect> ORD = CoreExpr.coreExprOrd.comap((x) -> x.target);
	public final CoreExpr target;

	public CoreExpr getTarget() {
		return this.target;
	}

	public Inspect(List<SourceFileRange> ranges, CoreExpr target) {
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
