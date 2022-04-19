package banjo.expr.core;

import banjo.expr.token.BadIdentifier;
import banjo.expr.token.Identifier;
import banjo.expr.token.NumberLiteral;
import banjo.expr.token.StringLiteral;

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
	public T listLiteral(ListLiteral n) {
		return fallback();
	}

	@Override
	public T badExpr(BadCoreExpr badExpr) {
		return fallback();
	}

	@Override
	public T badIdentifier(BadIdentifier n) {
		return fallback();
	}

	@Override
	public T extend(Extend n) {
		return fallback();
	}

		@Override
	public T scoped(ScopedExpr projection) {
		return fallback();
	}

    @Override
    public T kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
        return fallback();
    }
    
    @Override
    public T binding(BindingExpr b) {
    	return fallback();
    }
    @Override
    public T nil() {
        return fallback();
    }
}
