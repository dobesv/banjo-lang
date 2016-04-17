package banjo.eval.util;

import banjo.eval.environment.Environment;
import banjo.expr.free.FreeExpression;
import banjo.value.CalculatedValue;
import banjo.value.Value;
import banjo.value.ValueVisitor;
import fj.data.List;

/**
 * An expression plus an environment = a thunk
 */
public class LazyBoundValue extends CalculatedValue {
	public final FreeExpression expr;
	public final Environment environment;
	
	public LazyBoundValue(FreeExpression expr, Environment environment) {
		super();
		this.expr = expr;
		this.environment = environment;
	}
	
	@Override
	public Value calculate(List<Value> trace) {
        return expr.apply(environment, trace);
	}
	
	public LazyBoundValue updateEnv(Environment newEnvironment) {
		if(newEnvironment == this.environment)
			return this;
		return new LazyBoundValue(expr, newEnvironment);
	}

    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.lazyBoundValue(this);
    }
}

