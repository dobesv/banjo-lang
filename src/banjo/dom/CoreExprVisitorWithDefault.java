package banjo.dom;

import org.eclipse.jdt.annotation.Nullable;

public abstract class CoreExprVisitorWithDefault<T> implements CoreExprVisitor<T> {
	public abstract T fallback(CoreExpr unsupported);

	@Override
	public T visitStringLiteral(StringLiteral n) {
		return fallback(n);
	}

	@Override
	public T visitNumberLiteral(NumberLiteral n) {
		return fallback(n);
	}

	@Override
	public T visitIdentifier(Identifier n) {
		return fallback(n);
	}

	@Override
	public T visitCall(Call n) {
		return fallback(n);
	}

	@Override
	public T visitExprList(ExprList n) {
		return fallback(n);
	}

	@Override
	public T visitFieldRef(FieldRef n) {
		return fallback(n);
	}

	@Override
	public T visitFunctionLiteral(FunctionLiteral n) {
		return fallback(n);
	}

	@Override
	public T visitObjectLiteral(ObjectLiteral n) {
		return fallback(n);
	}

	@Override
	public T visitLet(Let n) {
		return fallback(n);
	}

	@Override
	public T visitListLiteral(ListLiteral n) {
		return fallback(n);
	}

	@Override
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
