package banjo.eval.environment;

import java.util.function.Function;

import banjo.eval.expr.BindingInstance;
import banjo.expr.core.CoreExpr;
import banjo.expr.free.FreeExpression;
import banjo.expr.free.FreeExpressionFactory;
import banjo.value.Reactive;
import banjo.value.Value;

public interface Environment extends Function<String, BindingInstance>, Reactive<Environment> {
	public default Value eval(CoreExpr ast) {
		return bind(FreeExpressionFactory.apply(ast));
	}

	public default Value bind(final FreeExpression fx) {
	    return fx.apply(this);
    }
}
