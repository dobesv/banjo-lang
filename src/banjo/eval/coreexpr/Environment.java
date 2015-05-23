package banjo.eval.coreexpr;

import java.util.function.Function;

import banjo.dom.core.CoreExpr;
import banjo.eval.util.JavaRuntimeSupport;

public interface Environment extends Function<String, BindingInstance>{
	public default Object eval(CoreExpr ast) {
		return JavaRuntimeSupport.force(bind(FreeExpressionFactory.apply(ast)));
	}

	public default Object bind(final FreeExpression fx) {
	    return fx.apply(this);
    }

}
