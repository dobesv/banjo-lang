package banjo.dom.source;

import org.eclipse.jdt.annotation.Nullable;

public interface SourceExprVisitor<T> extends AtomVisitor<T> {

	@Nullable T visitBinaryOp(BinaryOp op);
	@Nullable T visitUnaryOp(UnaryOp op);
	@Nullable T visitBadSourceExpr(BadSourceExpr badSourceExpr);
	@Nullable T visitEmpty(EmptyExpr emptyExpr);

}
