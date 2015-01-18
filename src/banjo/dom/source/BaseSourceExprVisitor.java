package banjo.dom.source;

import banjo.dom.core.MixfixFunctionIdentifier;
import banjo.dom.token.BadIdentifier;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;

public abstract class BaseSourceExprVisitor<T> implements SourceExprVisitor<T> {

	public abstract T fallback(SourceExpr other);

	@Override
	public T stringLiteral(StringLiteral stringLiteral) {
		return key(stringLiteral);
	}

	public T key(Key key) {
		return fallback(key);
	}

	@Override
	public T numberLiteral(NumberLiteral numberLiteral) {
		return key(numberLiteral);
	}

	public T token(SourceExpr token) {
		return fallback(token);
	}

	@Override
	public T identifier(Identifier identifier) {
		return key(identifier);
	}

	@Override
	public T mixfixFunctionIdentifier(MixfixFunctionIdentifier id) {
	    return key(id);
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
