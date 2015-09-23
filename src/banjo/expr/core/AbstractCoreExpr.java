package banjo.expr.core;

import banjo.expr.AbstractExpr;
import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public abstract class AbstractCoreExpr extends AbstractExpr implements CoreExpr {
	public AbstractCoreExpr(int hashCode, Set<SourceFileRange> sourceFileRanges) {
		super(sourceFileRanges);
	}
}
