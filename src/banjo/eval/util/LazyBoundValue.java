package banjo.eval.util;

import banjo.eval.Environment;
import banjo.event.Event;
import banjo.expr.free.FreeExpression;
import banjo.value.CalculatedValue;
import banjo.value.Reaction;
import banjo.value.Value;

public class LazyBoundValue extends CalculatedValue {
	public final FreeExpression expr;
	public final Environment environment;
	
	public LazyBoundValue(FreeExpression expr, Environment environment) {
		super();
		this.expr = expr;
		this.environment = environment;
	}
	
	@Override
	public Value calculate() {
		return expr.apply(environment);
	}
	
	@Override
	public boolean isCalculationReactive() {
		return environment.isReactive();
	}
	
	@Override
	public Reaction<Value> calculationReact(Event event) {
		return environment.react(event).map(this::updateEnv);
	}
	
	public Value updateEnv(Environment newEnvironment) {
		if(newEnvironment == this.environment)
			return this;
		return new LazyBoundValue(expr, newEnvironment);
	}
}

