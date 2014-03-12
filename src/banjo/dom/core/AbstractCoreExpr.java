package banjo.dom.core;

import banjo.dom.AbstractExpr;
import banjo.parser.util.SourceFileRange;

public abstract class AbstractCoreExpr extends AbstractExpr implements CoreExpr {
	public AbstractCoreExpr(int hashCode, SourceFileRange sourceFileRange) {
		super(hashCode, sourceFileRange);
	}
}
