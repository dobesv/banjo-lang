package banjo.value;

import java.util.Observable;

import banjo.event.Event;
import banjo.expr.util.ListUtil;
import fj.data.List;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;

public class MethodCallResultValue extends CalculatedValue {
	public final Value object;
	public final String name;
	public final Value targetObject;
	public final Value fallback;
	public final List<Value> args;
	public final boolean reactive;
	private ObservableMethodCallResultValue observable;
	
	public MethodCallResultValue(Value object, String name, Value targetObject, Value fallback, List<Value> args) {
		super();
		this.object = object;
		this.name = name;
		this.targetObject = targetObject;
		this.fallback = fallback;
		this.args = args;
		this.reactive = targetObject.isReactive() || (fallback != null && fallback.isReactive()) || args.exists(a -> a.isReactive());  
	}
	
	@Override
	public Value calculate() {
		return object.callMethod(name, targetObject, fallback, args);
	}
	
	@Override
	public Reaction<Value> calculationReact(Event event) {
		if(!reactive)
			return Reaction.of(this);
		return Reaction.p(Reaction.to(targetObject, event), Reaction.to(fallback, event), Reaction.to(args, event)).map(p -> this.update(p._1(), p._2(), p._3()));
	}
	
	public MethodCallResultValue update(Value targetObject, Value fallback, List<Value> args) {
		if(targetObject == this.targetObject && fallback == this.fallback && 
				ListUtil.elementsEq(args, this.args))
			return this;
		return new MethodCallResultValue(object, name, targetObject, fallback, args);
	}
	
	@Override
	public boolean isCalculationReactive() {
		return reactive;
	}
	
	public static final class ObservableMethodCallResultValue extends ObjectBinding<Value> {
		public final ObservableValue<Value> objectBinding;
		public final ObservableValue<Value> targetObjectBinding;
		public final ObservableValue<Value> fallbackBinding;
		public final List<ObservableValue<Value>> argsBinding;
		MethodCallResultValue methodCallResultValue;
		public ObservableMethodCallResultValue(MethodCallResultValue methodCallResultValue) {
			super();
			this.methodCallResultValue = methodCallResultValue;
			objectBinding = methodCallResultValue.object.toObservableValue();
			targetObjectBinding = methodCallResultValue.targetObject.toObservableValue();
			fallbackBinding = methodCallResultValue.fallback.toObservableValue();
			argsBinding = methodCallResultValue.args.map(Value::toObservableValue);
			bind(objectBinding, targetObjectBinding, fallbackBinding);
			argsBinding.forEach(this::bind);
		}
		
		@Override
		protected MethodCallResultValue computeValue() {
			return methodCallResultValue = methodCallResultValue.update(
					targetObjectBinding.getValue(),
					fallbackBinding.getValue(),
					argsBinding.map(ObservableValue::getValue));
		}
	}
	
	@Override
	public ObservableValue<Value> toObservableValue() {
		if(observable == null)
			observable = new ObservableMethodCallResultValue(this);
		return observable;
	}
}