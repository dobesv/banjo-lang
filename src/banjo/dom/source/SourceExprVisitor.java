package banjo.dom.source;

import banjo.dom.token.BadIdentifier;

public interface SourceExprVisitor<T> extends AtomVisitor<T> {

	T binaryOp(BinaryOp binaryOp);
	T unaryOp(UnaryOp unaryOp);
	T badSourceExpr(BadSourceExpr badSourceExpr);
	T emptyExpr(EmptyExpr emptyExpr);
	T badIdentifier(BadIdentifier badIdentifier);

}
