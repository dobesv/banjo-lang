package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.token.Atom;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;

/**
 * Provide a default base implementation of CoreExprVisitor that passes unhandled
 * expression types to a fallback.  Additionally, subclasses of Key and Atom can
 * be intercepted by overriding visitKey() and visitAtom().
 */
public abstract class BaseCoreExprVisitor<T> implements CoreExprVisitor<T> {
	@Nullable
	public abstract T fallback(CoreExpr unsupported);

	@Override
	@Nullable
	public T stringLiteral(StringLiteral n) {
		return key(n);
	}

	/**
	 * Called when visiting a subclass of Key and that subclass visit method has
	 * not been overridden.
	 */
	public @Nullable T key(Key key) {
		return atom(key);
	}

	/**
	 * Called when visiting a subclass of Atom and that subclass visit method has
	 * not been overridden.
	 */
	public @Nullable T atom(Atom atom) {
		return fallback(atom);
	}

	@Override
	@Nullable
	public T numberLiteral(NumberLiteral n) {
		return atom(n);
	}

	@Override
	@Nullable
	public T identifier(Identifier n) {
		return key(n);
	}

	@Override
	@Nullable
	public T call(Call n) {
		return fallback(n);
	}

	@Override
	@Nullable
	public T exprList(ExprList n) {
		return fallback(n);
	}

	@Override
	@Nullable
	public T projection(Projection n) {
		return fallback(n);
	}

	@Override
	@Nullable
	public T functionLiteral(FunctionLiteral n) {
		return fallback(n);
	}

	@Override
	@Nullable
	public T objectLiteral(ObjectLiteral n) {
		return fallback(n);
	}

	@Override
	@Nullable
	public T let(Let n) {
		return fallback(n);
	}

	@Override
	@Nullable
	public T listLiteral(ListLiteral n) {
		return fallback(n);
	}

	@Override
	@Nullable
	public T setLiteral(SetLiteral n) {
		return fallback(n);
	}

	@Override
	@Nullable
	public T badExpr(BadExpr n) {
		return fallback(n);
	}

	@Override
	@Nullable
	public T operator(OperatorRef n) {
		return fallback(n);
	}
}
