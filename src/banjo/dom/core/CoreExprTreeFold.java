package banjo.dom.core;

import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.Nullable;

import fj.F2;
import fj.data.List;

/**
 * Goal: walk the tree and extract information, like a list of errors.  It's a kind of fold
 * over the tree.
 * 
 * A subclass can extend this class to capture nodes from the tree of its choice.
 */
public class CoreExprTreeFold<T extends CoreExprTreeFold<T>> extends BaseCoreExprVisitor<T> {
	@SuppressWarnings("unchecked")
	T self = (T)this;

	@Override
	@Nullable
	public T call(Call n) {
		final T v1 = fold(n.getArguments());
		final T v2 = nonNull(n.getObject().acceptVisitor(v1));
		return v2;
	}

	public T fold(final List<CoreExpr> args) {
		final T v1 = nonNull(args.foldRight(new F2<CoreExpr,T,T>() {
			@Override
			public T f(CoreExpr a, T b) {
				return nonNull(a.acceptVisitor(b));
			}
		}, this.self));
		return v1;
	}

	@Override
	public T extend(Extend n) {
		final T v1 = nonNull(n.getExtension().acceptVisitor(this));
		final T v2 = nonNull(n.getBase().acceptVisitor(v1));
		return v2;
	}

	@Override
	public T inspect(Inspect n) {
		return nonNull(n.getTarget().acceptVisitor(this));
	}

	@Override
	public T listLiteral(ListLiteral n) {
		return fold(n.getElements());
	}

	public T methodParamDecl(MethodParamDecl p) {
		final T v1 = nonNull(p.getAssertion().acceptVisitor(this.self));
		final T v2 = nonNull(p.getName().acceptVisitor(v1));
		return v2;
	}

	public T method(Method m) {
		final T v1 = nonNull(m.getBody().acceptVisitor(this.self));
		final T v2 = nonNull(m.getGuarantee().acceptVisitor(v1));
		final T v3 = nonNull(m.getArgs().foldRight(new F2<MethodParamDecl,T,T>() {
			@Override
			public T f(MethodParamDecl a, T b) {
				return b.methodParamDecl(a);
			}
		}, v2));
		return v3;
	}

	@Override
	@Nullable
	public T objectLiteral(ObjectLiteral n) {
		return n.getMethods().foldRight(new F2<Method,T,T>() {
			@Override
			public T f(Method a, T b) {
				return b.method(a);
			}
		}, this.self);
	}


	@Override
	@Nullable
	public T fallback(CoreExpr unsupported) {
		return this.self;
	}

}
