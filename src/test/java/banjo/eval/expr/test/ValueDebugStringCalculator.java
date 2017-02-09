package banjo.eval.expr.test;

import banjo.eval.resolver.InstanceAlgebra;
import banjo.eval.resolver.Resolver;
import banjo.expr.core.BaseCoreExprVisitor;
import banjo.expr.core.Call;
import banjo.expr.core.CoreExpr;
import banjo.expr.core.Let;
import banjo.expr.core.Projection;
import banjo.expr.source.Operator;
import banjo.expr.util.ListUtil;
import banjo.value.Value;
import fj.P;

public class ValueDebugStringCalculator<T> extends BaseCoreExprVisitor<String> {
    public final Resolver<T> resolver;
    public final InstanceAlgebra<T> algebra;
    public final CoreExpr expr;
    public final Value value;
	
    public ValueDebugStringCalculator(Resolver<T> resolver, InstanceAlgebra<T> algebra, CoreExpr expr, Value value) {
		super();
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
	public String call(Call n) {
		if(n.target instanceof Projection) {
			final Projection methodReceiver = (Projection)n.target;
            final T lhs = resolver.eval(methodReceiver.object, algebra);
			return n.getBinaryOperator().map(x -> {
                final Object rhs = resolver.eval(n.args.head(), algebra);
				return "(" + n.toSource() + ") == ("+lhs+" "+x.getOp()+" "+rhs+")"+ " --> " + value;
			}).orSome(P.lazy(() -> "(" +
                    n.toSource() +
                    ") == (" +
                    lhs +
                    "." +
                    methodReceiver.body +
                    "(" +
                ListUtil.insertCommas(n.args.toStream().map(arg -> resolver.eval(arg, algebra))) +
                    ")" +
                    "==" +
                value
			));
		}
	    return fallback();
	}

	@Override
	public String let(Let let) {
//		List<P2<String, FreeExpression>> bindings = let.bindings
//				.map(P2.map2_(e -> e.acceptVisitor(FreeExpressionFactory.INSTANCE)))
//				.map(P2.map1_(e -> e.id));
//        PartialResolver partialResolver = FreeExpressionFactory.letPartialResolver(TreeMap.iterableTreeMap(Ord.stringOrd, bindings));
//        FreeExpression freeBody = FreeExpressionFactory.apply(let.body);
//        FreeExpression boundBody = freeBody.partial(partialResolver).orSome(freeBody);
        return let.body.acceptVisitor(new ValueDebugStringCalculator<T>(resolver, algebra, expr, value));
	}
}