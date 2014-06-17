package banjo.dom.core;

import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.core.Call.MessagePart;
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
		final T v1 = foldCallParts(n.getParts());
		final T v2 = nonNull(n.getObject().acceptVisitor(v1));
		return v2;
	}

	public T foldCallParts(final List<Call.MessagePart> parts) {
		final T v1 = nonNull(parts.foldRight(new F2<Call.MessagePart,T,T>() {
			@Override
			public @Nullable T f(@Nullable Call.MessagePart a, @Nullable T b) {
				T v1 = nonNull(b).fold(nonNull(a).getArguments());
				T v2 = nonNull(a).getKey().acceptVisitor(v1);
				return v2;
			}
		}, this.self));
		return v1;
	}
	public T fold(final List<CoreExpr> args) {
		final T v1 = nonNull(args.foldRight(new F2<CoreExpr,T,T>() {
			@Override
			public @Nullable T f(@Nullable CoreExpr a, @Nullable T b) {
				return nonNull(a).acceptVisitor(nonNull(b));
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

	public T methodParamDecl(MethodFormalArgument p) {
		final T v1 = nonNull(p.getAssertion().acceptVisitor(this.self));
		final T v2 = nonNull(p.getName().acceptVisitor(v1));
		return v2;
	}

	public T foldMethodArguments(List<MethodFormalArgument> arguments) {
		return nonNull(arguments.foldRight(new F2<MethodFormalArgument,T,T>() {
			@Override
			public T f(@Nullable MethodFormalArgument a, @Nullable T b) {
				return nonNull(b).methodParamDecl(nonNull(a));
			}
		}, this.self));
	}
	public T foldMethodParts(final List<Method.SignaturePart> parts) {
		final T v1 = nonNull(parts.foldRight(new F2<Method.SignaturePart,T,T>() {
			@Override
			public @Nullable T f(@Nullable Method.SignaturePart a, @Nullable T b) {
				T v1 = nonNull(b).foldMethodArguments(nonNull(a).getArguments());
				T v2 = nonNull(a).getKey().acceptVisitor(v1);
				return v2;
			}
		}, this.self));
		return v1;
	}
	
	public T method(Method m) {
		final T v1 = nonNull(m.getBody().acceptVisitor(this.self));
		final T v2 = nonNull(m.getGuarantee().acceptVisitor(v1));
		final T v3 = v2.foldMethodParts(m.getParts());
		return v3;
	}

	@Override
	@Nullable
	public T objectLiteral(ObjectLiteral n) {
		return n.getMethods().foldRight(new F2<Method,T,T>() {
			@Override
			public T f(@Nullable Method a, @Nullable T b) {
				return nonNull(b).method(nonNull(a));
			}
		}, this.self);
	}


	@Override
	@Nullable
	public T fallback(CoreExpr unsupported) {
		return this.self;
	}

}
