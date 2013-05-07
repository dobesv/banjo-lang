package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Expr;
import banjo.parser.util.OffsetExpr;

public class OffsetCoreExpr extends OffsetExpr<CoreExpr> implements CoreExpr {

	public OffsetCoreExpr(int offset, CoreExpr value) {
		super(offset, value);
	}

	@Override
	public int getSourceLength() {
		return getValue().getSourceLength();
	}

	@Override
	@Nullable
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return getValue().acceptVisitor(visitor);
	}

	@Override
	public Class<? extends Expr> getExprClass() {
		return getValue().getExprClass();
	}

}
