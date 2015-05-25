package banjo.eval.coreexpr;

import java.util.function.Function;

import banjo.eval.util.JavaRuntimeSupport;
import banjo.expr.core.CoreExpr;
import banjo.expr.free.FreeExpression;
import banjo.expr.free.FreeExpressionFactory;

public interface Environment extends Function<String, BindingInstance>{
	public default Object eval(CoreExpr ast) {
		return JavaRuntimeSupport.force(bind(FreeExpressionFactory.apply(ast)));
	}

	public default Object bind(final FreeExpression fx) {
	    return fx.apply(this);
    }

}
