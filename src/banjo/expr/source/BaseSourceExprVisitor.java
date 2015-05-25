package banjo.expr.source;

import banjo.expr.token.BadIdentifier;
import banjo.expr.token.Identifier;
import banjo.expr.token.NumberLiteral;
import banjo.expr.token.OperatorRef;
import banjo.expr.token.StringLiteral;

public abstract class BaseSourceExprVisitor<T> implements SourceExprVisitor<T> {

	public abstract T fallback(SourceExpr other);

	@Override
	public T stringLiteral(StringLiteral stringLiteral) {
		return fallback(stringLiteral);
	}

	@Override
	public T numberLiteral(NumberLiteral numberLiteral) {
		return fallback(numberLiteral);
	}

	public T token(SourceExpr token) {
		return fallback(token);
	}

	@Override
	public T identifier(Identifier identifier) {
		return fallback(identifier);
	}

	@Override
	public T operator(OperatorRef operatorRef) {
		return fallback(operatorRef);
	}

	@Override
	public T binaryOp(BinaryOp op) {
		return fallback(op);
	}

	@Override
	public T unaryOp(UnaryOp op) {
		return fallback(op);
	}

	@Override
	public T badSourceExpr(BadSourceExpr badSourceExpr) {
		return fallback(badSourceExpr);
	}

	@Override
	public T badIdentifier(BadIdentifier badIdentifier) {
		return fallback(badIdentifier);
	}

	@Override
	public T emptyExpr(EmptyExpr emptyExpr) {
		return fallback(emptyExpr);
	}
}
