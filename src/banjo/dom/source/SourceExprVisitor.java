package banjo.dom.source;

import org.eclipse.jdt.annotation.Nullable;

public interface SourceExprVisitor<T> extends AtomVisitor<T> {

	@Nullable T binaryOp(BinaryOp binaryOp);
	@Nullable T unaryOp(UnaryOp unaryOp);
	@Nullable T badSourceExpr(BadSourceExpr badSourceExpr);
	@Nullable T emptyExpr(EmptyExpr emptyExpr);

}
