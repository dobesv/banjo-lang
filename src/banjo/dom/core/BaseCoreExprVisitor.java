package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import fj.data.List;
import banjo.dom.BadExpr;
import banjo.dom.token.Atom;
import banjo.dom.token.BadIdentifier;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;
import banjo.parser.util.SourceFileRange;

/**
 * Provide a default base implementation of CoreExprVisitor that passes unhandled
 * expression types to a fallback.  Additionally, subclasses of Key and Atom can
 * be intercepted by overriding visitKey() and visitAtom().
 */
public abstract class BaseCoreExprVisitor<T> implements CoreExprVisitor<T> {
	public abstract T fallback();

	@Override
	public T stringLiteral(StringLiteral n) {
		return key(n);
	}

	/**
	 * Called when visiting a subclass of Key and that subclass visit method has
	 * not been overridden.
	 */
	public T key(Key key) {
		return fallback();
	}

	@Override
	public T numberLiteral(NumberLiteral n) {
		return fallback();
	}

	@Override
	public T identifier(Identifier n) {
		return key(n);
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
	public T method(Method method) {
		return fallback();
	}

	@Override
	public T anonymous() {
		return fallback();
	}

	@Override
	public T mixfixFunctionIdentifier(MixfixFunctionIdentifier mixfixFunctionIdentifier) {
		return fallback();
	}

	@Override
	public T alternativeDefinition(AlternativeDefinition alternativeDefinition) {
		return fallback();
	}
}
