package banjo.dom.core;

import banjo.dom.BadExpr;
import banjo.dom.token.BadIdentifier;
import banjo.dom.token.Identifier;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;

/**
 * Provide a default base implementation of CoreExprVisitor that passes unhandled
 * expression types to a fallback.  Additionally, subclasses of Key and Atom can
 * be intercepted by overriding visitKey() and visitAtom().
 */
public abstract class BaseCoreExprVisitor<T> implements CoreExprVisitor<T> {
	public abstract T fallback();

	@Override
	public T stringLiteral(StringLiteral n) {
		return fallback();
	}

	@Override
	public T numberLiteral(NumberLiteral n) {
		return fallback();
	}

	@Override
	public T identifier(Identifier n) {
		return fallback();
	}

	@Override
	public T call(Call n) {
		return fallback();
	}

	@Override
	public T objectLiteral(ObjectLiteral n) {
		return fallback();
	}

	@Override
	public T listLiteral(ListLiteral n) {
		return fallback();
	}

	@Override
	public T badExpr(BadExpr badExpr) {
		return fallback();
	}

	@Override
	public T badIdentifier(BadIdentifier n) {
		return fallback();
	}

	@Override
	public T operator(OperatorRef n) {
		return fallback();
	}

	@Override
	public T inspect(Inspect n) {
		return fallback();
	}

	@Override
	public T extend(Extend n) {
		return fallback();
	}

	@Override
	public T let(Let let) {
	    return fallback();
	}

	@Override
	public T functionLiteral(FunctionLiteral f) {
	    return fallback();
	}

}
