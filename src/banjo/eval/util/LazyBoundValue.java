package banjo.eval.util;

import java.util.function.BiFunction;

import banjo.eval.environment.Environment;
import banjo.event.PastEvent;
import banjo.expr.free.FreeExpression;
import banjo.value.CalculatedValue;
import banjo.value.Reaction;
import banjo.value.Reactive;
import banjo.value.Value;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;

/**
 * An expression plus an environment = a thunk
 */
public class LazyBoundValue extends CalculatedValue {
	public final FreeExpression expr;
	public final Environment environment;
	private ObservableLazyBoundValue observable;
	
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
	public Reaction<Value> calculationReact(PastEvent event) {
		return environment.react(event).map(this::updateEnv);
	}
	
	public LazyBoundValue updateEnv(Environment newEnvironment) {
		if(newEnvironment == this.environment)
			return this;
		return new LazyBoundValue(expr, newEnvironment);
	}

	public static final class ObservableLazyBoundValue extends ObjectBinding<Value> {
		final ObservableValue<Environment> environmentBinding;
		LazyBoundValue lazyBoundValue;
		
		public ObservableLazyBoundValue(LazyBoundValue lazyBoundValue) {
			super();
			environmentBinding = lazyBoundValue.environment.toObservableValue();
			bind(environmentBinding);
			this.lazyBoundValue = lazyBoundValue;
		}
		
		@Override
		public void dispose() {
			unbind(environmentBinding);
		}
		
		@Override
		protected Value computeValue() {
			return lazyBoundValue = lazyBoundValue.updateEnv(environmentBinding.getValue());
		}
		
	}
	@Override
	public ObservableValue<Value> toObservableValue() {
		if(observable == null)
			observable = new ObservableLazyBoundValue(this);
		return observable;
	}

}

