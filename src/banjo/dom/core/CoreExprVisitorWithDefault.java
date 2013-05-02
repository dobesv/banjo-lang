package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.token.Identifier;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;

public abstract class CoreExprVisitorWithDefault<T> implements CoreExprVisitor<T> {
	@Nullable
	public abstract T fallback(CoreExpr unsupported);

	@Override
	@Nullable
	public T visitStringLiteral(StringLiteral n) {
		return fallback(n);
	}

	@Override
	@Nullable
	public T visitNumberLiteral(NumberLiteral n) {
		return fallback(n);
	}

	@Override
	@Nullable
	public T visitIdentifier(Identifier n) {
		return fallback(n);
	}

	@Override
	@Nullable
	public T visitCall(Call n) {
		return fallback(n);
	}

	@Override
	@Nullable
	public T visitExprList(ExprList n) {
		return fallback(n);
	}

	@Override
	@Nullable
	public T visitFieldRef(FieldRef n) {
		return fallback(n);
	}

	@Override
	@Nullable
	public T visitFunctionLiteral(FunctionLiteral n) {
		return fallback(n);
	}

	@Override
	@Nullable
	public T visitObjectLiteral(ObjectLiteral n) {
		return fallback(n);
	}

	@Override
	@Nullable
	public T visitLet(Let n) {
		return fallback(n);
	}

	@Override
	@Nullable
	public T visitListLiteral(ListLiteral n) {
		return fallback(n);
	}

	@Override
	@Nullable
	public T visitSetLiteral(SetLiteral n) {
		return fallback(n);
	}
	
	@Override
	@Nullable
	public T visitBadExpr(BadExpr n) {
		return fallback(n);
	}

	@Override
	@Nullable
	public T visitOperator(OperatorRef n) {
		return fallback(n);
	}
}
