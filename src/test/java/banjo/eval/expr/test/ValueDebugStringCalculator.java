package banjo.eval.expr.test;

import banjo.eval.resolver.InstanceAlgebra;
import banjo.eval.resolver.Resolver;
import banjo.expr.core.BaseCoreExprVisitor;
import banjo.expr.core.CoreExpr;
import banjo.expr.core.Let;
import banjo.expr.core.ScopedExpr;
import banjo.expr.source.Operator;
import banjo.expr.util.ListUtil;
import banjo.value.Value;
import fj.F;
import fj.P;

public class ValueDebugStringCalculator<T> extends BaseCoreExprVisitor<String> {
	public final Resolver<T> resolver;
	public final InstanceAlgebra<T> algebra;
	public final CoreExpr expr;
	public final Value value;
	public final T projectRoot;

	public ValueDebugStringCalculator(T projectRoot, Resolver<T> resolver, InstanceAlgebra<T> algebra, CoreExpr expr,
			Value value) {
		super();
		this.projectRoot = projectRoot;
		this.resolver = resolver;
		this.algebra = algebra;
		this.expr = expr;
		this.value = value;
	}

	@Override
	public String fallback() {
		return expr.toSource(Operator.EQ.getRightPrecedence()) + " == " + value.toString();
	}

	@Override
	public String scoped(ScopedExpr projection) {
		if (projection.isCall()) {
			return projection.getObject().acceptVisitor(new BaseCoreExprVisitor<String>() {
				@Override
				public String fallback() {
					return ValueDebugStringCalculator.super.scoped(projection);
				}

				@Override
				public String scoped(ScopedExpr methodReceiver) {
					final T lhs = resolver.eval(methodReceiver.getObject(), projectRoot, algebra);
					return projection.getBinaryOperator().<String>map(x -> {
						final Object rhs = resolver.eval(projection.getArgs().head(), projectRoot, algebra);
						return "(" + projection.toSource() + ") == (" + lhs + " " + x.getOp() + " " + rhs + ")"
								+ " --> " + value;
					}).orSome(P.lazy(() -> "(" + projection.toSource() + ") == (" + lhs + "." + methodReceiver.getBody() + "("
							+ ListUtil.insertCommas(
									projection.getArgs().toStream().map(arg -> resolver.eval(arg, projectRoot, algebra)))
							+ ")" + "==" + value));
				}
			});
		}
		return super.scoped(projection);
	}

	@Override
	public String let(Let let) {
//		List<P2<String, FreeExpression>> bindings = let.bindings
//				.map(P2.map2_(e -> e.acceptVisitor(FreeExpressionFactory.INSTANCE)))
//				.map(P2.map1_(e -> e.id));
//        PartialResolver partialResolver = FreeExpressionFactory.letPartialResolver(TreeMap.iterableTreeMap(Ord.stringOrd, bindings));
//        FreeExpression freeBody = FreeExpressionFactory.apply(let.body);
//        FreeExpression boundBody = freeBody.partial(partialResolver).orSome(freeBody);
		return let.body.acceptVisitor(new ValueDebugStringCalculator<T>(projectRoot, resolver, algebra, expr, value));
	}
}