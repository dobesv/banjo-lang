package banjo.dom;

import org.eclipse.jdt.annotation.Nullable;

public interface SourceExprVisitor<T> extends AtomVisitor<T> {

	@Nullable T visitBinaryOp(BinaryOp op);
	@Nullable T visitUnaryOp(UnaryOp op);
	@Nullable T visitBadExpr(BadExpr badExpr);
	
}
