package banjo.expr.core;

import banjo.expr.AbstractExpr;
import banjo.expr.util.SourceFileRange;
import fj.data.List;

public abstract class AbstractCoreExpr extends AbstractExpr implements CoreExpr {
	public AbstractCoreExpr(int hashCode, List<SourceFileRange> sourceFileRanges) {
		super(sourceFileRanges);
	}
}
