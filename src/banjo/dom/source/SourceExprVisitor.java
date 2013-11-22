package banjo.dom.source;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.token.BadIdentifier;

public interface SourceExprVisitor<T> extends AtomVisitor<T> {

	@Nullable T binaryOp(BinaryOp binaryOp);
	@Nullable T unaryOp(UnaryOp unaryOp);
	@Nullable T badSourceExpr(BadSourceExpr badSourceExpr);
	@Nullable T emptyExpr(EmptyExpr emptyExpr);
	@Nullable T badIdentifier(BadIdentifier badIdentifier);

}
