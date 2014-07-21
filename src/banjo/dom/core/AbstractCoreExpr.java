package banjo.dom.core;

import fj.data.List;
import banjo.dom.AbstractExpr;
import banjo.parser.util.SourceFileRange;

public abstract class AbstractCoreExpr extends AbstractExpr implements CoreExpr {
	public AbstractCoreExpr(int hashCode, List<SourceFileRange> sourceFileRanges) {
		super(hashCode, sourceFileRanges);
	}
}
