package banjo.eval.util;

import java.lang.reflect.Constructor;
import java.lang.reflect.Executable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import banjo.eval.Fail;
import banjo.eval.value.FunctionTrait;
import banjo.eval.value.FunctionValue;
import banjo.eval.value.Value;
import banjo.eval.value.ValueToStringTrait;
import fj.data.Either;
import fj.data.List;

public class OverloadedJavaMethodCaller extends FunctionTrait implements Value {
	public final Object target;
	public final Method[] methods;
	public OverloadedJavaMethodCaller(Object target, Method[] methods) {
        super();
        this.target = target;
        this.methods = methods;
    }

	@Override
	public Value call(Value recurse, Value prevImpl, List<Value> arguments) {
		return callJavaMethod(target, methods, arguments);
	}

	@Override
	public String toStringFallback() {
	    return methods[0].getDeclaringClass().getName()+"."+methods[0].getName();
	}

	public static Value callJavaMethod(Object target, Executable[] methods, List<Value> arguments) {
		Value[] argumentArray = arguments.array(Value[].class);
		int argumentCount = argumentArray.length;
		methodLoop: for(Executable method : methods) {
			final int parameterCount = method.getParameterCount();
			if(argumentCount < parameterCount)
				continue;

			Class<?>[] paramTypes = method.getParameterTypes();
			Object[] convertedParams = new Object[parameterCount];
			for(int i = 0; i < parameterCount; i++) {
				try {
					final Either<?,Fail> conversion =
							paramTypes[i].isInstance(argumentArray[i]) ?
									Either.left(argumentArray[i]) :
									argumentArray[i].convertToJava(paramTypes[i]);
					if(conversion.isRight())
						continue methodLoop; // No good
					convertedParams[i] = conversion.left().value();
				} catch(Exception e) {
					// Failed to convert
					continue methodLoop;
				}
			}

			try {
				method.setAccessible(true);
				Object result;
				if(method instanceof Constructor) {
					final Constructor<?> ctor = (Constructor<?>)method;
					result = ctor.newInstance(convertedParams);
				} else {
					final Method instanceMethod = (Method)method;
					result = instanceMethod.invoke(target, convertedParams);
				}
				return (result instanceof Value) ?
					(Value)result :
					Value.fromJava(result);
	        } catch (InvocationTargetException | InstantiationException | IllegalAccessException | IllegalArgumentException e) {
	        	return new Fail(e);
	        }
		}
		return new Fail(new NoSuchMethodError("Failed to find a compatible method: "+target+"."+methods[0].getName()));
	}
}