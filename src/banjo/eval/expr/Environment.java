package banjo.eval.expr;

import java.util.function.Function;

import banjo.eval.value.Value;
import banjo.expr.core.CoreExpr;
import banjo.expr.free.FreeExpression;
import banjo.expr.free.FreeExpressionFactory;

public interface Environment extends Function<String, BindingInstance>{
	public default Value eval(CoreExpr ast) {
		return bind(FreeExpressionFactory.apply(ast));
	}

	public default Value bind(final FreeExpression fx) {
	    return fx.apply(this);
    }

}
