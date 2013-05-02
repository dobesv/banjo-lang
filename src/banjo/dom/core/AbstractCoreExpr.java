package banjo.dom.core;

import banjo.dom.AbstractExpr;
import banjo.dom.source.SourceExpr;

public abstract class AbstractCoreExpr extends AbstractExpr implements CoreExpr {
	private final SourceExpr sourceExpr;

	public AbstractCoreExpr(SourceExpr sourceExpr) {
		super();
		this.sourceExpr = sourceExpr;
	}

	public SourceExpr getSourceExpr() {
		return sourceExpr;
	}
	
}
