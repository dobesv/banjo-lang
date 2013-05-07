package banjo.dom.core;

import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.AbstractExpr;
import banjo.dom.Expr;
import banjo.dom.source.SourceExpr;

public abstract class AbstractCoreExpr extends AbstractExpr implements CoreExpr {
	private final int sourceLength;

	public AbstractCoreExpr(SourceExpr sourceExpr, int hashCode) {
		this(sourceExpr.getSourceLength(), hashCode);
	}

	public AbstractCoreExpr(int sourceLength, int hashCode) {
		super(sourceLength*13 + hashCode);
		this.sourceLength = sourceLength;
	}

	@Override
	public int getSourceLength() {
		return this.sourceLength;
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		return ((obj instanceof AbstractCoreExpr) && ((AbstractCoreExpr) obj).sourceLength == this.sourceLength);
	}

	@Override
	public Class<? extends Expr> getExprClass() {
		return nonNull(getClass());
	}

}
