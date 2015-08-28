package banjo.eval.util;

import java.lang.reflect.Executable;

import banjo.event.Event;
import banjo.value.CalculatedValue;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.data.List;

/**
 * Denotes the result of a java method call. 
 */
public class JavaMethodCall extends CalculatedValue implements Value {
	final Object javaObject;
	final Executable[] methods;
	final List<Value> arguments;
	
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
		if(newArguments == arguments)
			return this;
		return new JavaMethodCall(javaObject, methods, newArguments);
	}

}