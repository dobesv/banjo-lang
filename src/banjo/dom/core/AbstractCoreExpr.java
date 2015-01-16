package banjo.dom.core;

import banjo.dom.AbstractExpr;
import banjo.parser.util.SourceFileRange;
import fj.data.List;

public abstract class AbstractCoreExpr extends AbstractExpr implements CoreExpr {
	public AbstractCoreExpr(int hashCode, List<SourceFileRange> sourceFileRanges) {
		super(hashCode, sourceFileRanges);
	}
}
