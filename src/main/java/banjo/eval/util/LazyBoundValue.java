package banjo.eval.util;

import banjo.eval.resolver.Resolver;
import banjo.eval.resolver.ValueInstanceAlgebra;
import banjo.expr.free.FreeExpression;
import banjo.value.CalculatedValue;
import banjo.value.Value;
import fj.data.List;

/**
 * An expression plus an environment = a thunk
 */
public class LazyBoundValue extends CalculatedValue {
	public final FreeExpression expr;
    public final Resolver<Value> resolver;
	
    public LazyBoundValue(FreeExpression expr, Resolver<Value> resolver) {
		super();
		this.expr = expr;
        this.resolver = resolver;
	}
	
	@Override
	public Value calculate(List<Value> trace) {
        return expr.eval(trace, resolver, ValueInstanceAlgebra.INSTANCE);
	}
}

