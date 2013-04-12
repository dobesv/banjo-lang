package banjo.dom;

public interface ParseTreeVisitor<T> {

	T visitBinaryOp(BinaryOp op);
	T visitUnaryOp(UnaryOp op);
	T visitStringLiteral(StringLiteral stringLiteral);
	T visitNumberLiteral(NumberLiteral numberLiteral);
	T visitSimpleName(SimpleName simpleName);
	T visitEllipsis(Ellipsis ellipsis);
	T visitUnit(UnitRef unit);
	T visitOperator(OperatorRef operatorRef);
}
