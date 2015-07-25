package banjo.eval.util;

import java.lang.reflect.Executable;

import banjo.event.Event;
import banjo.value.CalculatedValue;
import banjo.value.JavaObjectValue;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.data.List;

public class JavaMethodCallResult extends CalculatedValue implements Value {
	final Object javaObject;
	final Executable[] methods;
	final List<Value> arguments;
	
	public JavaMethodCallResult(Object javaObject, Executable[] methods, List<Value> arguments) {
		this.javaObject = javaObject;
		this.methods = methods;
		this.arguments = arguments;
	}

	class JavaMethodCallReactiveResult extends JavaObjectValue {
		public JavaMethodCallReactiveResult(Object result) {
			super(result);
		}
		
		@Override
		public Reaction<Value> react(Event event) {
			return JavaMethodCallResult.this.react(event).map(this::update);
		}
		
		public Value update(Value newJmcr) {
			if(newJmcr == JavaMethodCallResult.this) {
				return this;
			}
			return newJmcr;
		}
		
		@Override
		public boolean isReactive() {
			return true;
		}
	}
	
	public Value wrapResult(Object result) {
		Value inertValue = Value.fromJava(result);
		if(this.isReactive()) {
			return new JavaMethodCallReactiveResult(inertValue);
		} else {
			return inertValue;
		}
	}
	
	@Override
	public Value calculate() {
		return JavaMethodCaller.callJavaMethod(javaObject, methods, arguments, this::wrapResult);
	}
	
	@Override
	public Reaction<Value> calculationReact(Event event) {
		return Reaction.to(arguments, event).map(this::updateArguments); 
	}
	
	@Override
	public boolean isCalculationReactive() {
		return arguments.exists(arg -> arg.isReactive());
	}
	
	public JavaMethodCallResult updateArguments(List<Value> newArguments) {
		if(newArguments == arguments)
			return this;
		return new JavaMethodCallResult(javaObject, methods, newArguments);
	}

}