package banjo.dom.core;

import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.AbstractExpr;
import banjo.dom.Expr;
import banjo.dom.source.SourceExpr;

public abstract class AbstractCoreExpr extends AbstractExpr implements CoreExpr {
	private final SourceExpr sourceExpr;

	public AbstractCoreExpr(SourceExpr sourceExpr, int hashCode) {
		super(sourceExpr.hashCode()*13 + hashCode);
		this.sourceExpr = sourceExpr;
	}

	@Override
	public SourceExpr getSourceExpr() {
		return this.sourceExpr;
	}

	@Override
	public int getSourceLength() {
		return this.sourceExpr.getSourceLength();
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		return ((obj instanceof AbstractCoreExpr) && ((AbstractCoreExpr) obj).getSourceExpr().equals(getSourceExpr()));
	}

	@Override
	public Class<? extends Expr> getExprClass() {
		return nonNull(getClass());
	}

}
