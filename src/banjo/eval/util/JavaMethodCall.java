package banjo.eval.util;

import java.lang.reflect.Executable;

import banjo.event.Event;
import banjo.expr.util.ListUtil;
import banjo.value.CalculatedValue;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.data.List;
import javafx.beans.Observable;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;

/**
 * Denotes the result of a java method call. 
 */
public class JavaMethodCall extends CalculatedValue implements Value {
	final Object javaObject;
	final Executable[] methods;
	final List<Value> arguments;
	private ObservableJavaMethodCall observable;
	
	public JavaMethodCall(Object javaObject, Executable[] methods, List<Value> arguments) {
		this.javaObject = javaObject;
		this.methods = methods;
		this.arguments = arguments;
	}

	@Override
	public Value calculate() {
		return JavaMethodValue.callJavaMethod(javaObject, methods, arguments);
	}
	
	@Override
	public Reaction<Value> calculationReact(Event event) {
		return Reaction.to(arguments, event).map(this::updateArguments); 
	}
	
	@Override
	public boolean isCalculationReactive() {
		return arguments.exists(arg -> arg.isReactive());
	}
	
	public JavaMethodCall updateArguments(List<Value> newArguments) {
		if(ListUtil.elementsEq(newArguments, arguments))
			return this;
		return new JavaMethodCall(javaObject, methods, newArguments);
	}

	public static final class ObservableJavaMethodCall extends ObjectBinding<Value> {
		final List<ObservableValue<Value>> argBindings;
		JavaMethodCall javaMethodCall;
		
		public ObservableJavaMethodCall(JavaMethodCall javaMethodCall) {
			super();
			argBindings = javaMethodCall.arguments.map(Value::toObservableValue);
			bind(argBindings.map(Observable.class::cast).array(Observable[].class));
			this.javaMethodCall = javaMethodCall;
		}
		
		@Override
		public void dispose() {
			unbind(argBindings.map(Observable.class::cast).array(Observable[].class));
		}
		
		@Override
		protected Value computeValue() {
			return javaMethodCall = javaMethodCall.updateArguments(argBindings.map(ObservableValue::getValue));
		}
		
	}
	
	@Override
	public ObservableValue<Value> toObservableValue() {
		if(observable == null)
			observable = new ObservableJavaMethodCall(this);
		return observable;
	}
}