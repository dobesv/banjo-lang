package banjo.dom.source;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.token.Atom;
import banjo.dom.token.BadIdentifier;
import banjo.dom.token.Ellipsis;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;

public abstract class BaseSourceExprVisitor<T> implements SourceExprVisitor<T> {

	@Nullable public abstract T fallback(SourceExpr other);

	@Override
	@Nullable
	public T stringLiteral(StringLiteral stringLiteral) {
		return key(stringLiteral);
	}

	@Nullable
	public T key(Key key) {
		return atom(key);
	}

	@Nullable
	public T atom(Atom atom) {
		return token(atom);
	}

	@Override
	@Nullable
	public T numberLiteral(NumberLiteral numberLiteral) {
		return atom(numberLiteral);
	}

	@Override
	@Nullable
	public T ellipsis(Ellipsis ellipsis) {
		return token(ellipsis);
	}

	@Nullable
	public T token(SourceExpr token) {
		return fallback(token);
	}

	@Override
	@Nullable
	public T identifier(Identifier identifier) {
		return key(identifier);
	}

	@Override
	@Nullable
	public T operator(OperatorRef operatorRef) {
		return atom(operatorRef);
	}

	@Override
	@Nullable
	public T binaryOp(BinaryOp op) {
		return fallback(op);
	}

	@Override
	@Nullable
	public T unaryOp(UnaryOp op) {
		return fallback(op);
	}

	@Override
	@Nullable
	public T badSourceExpr(BadSourceExpr badSourceExpr) {
		return fallback(badSourceExpr);
	}

	@Override
	@Nullable
	public T badIdentifier(BadIdentifier badIdentifier) {
		return fallback(badIdentifier);
	}

	@Override
	@Nullable
	public T emptyExpr(EmptyExpr emptyExpr) {
		return fallback(emptyExpr);
	}

}
