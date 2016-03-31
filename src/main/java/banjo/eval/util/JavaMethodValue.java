package banjo.eval.util;

import java.lang.reflect.Constructor;
import java.lang.reflect.Executable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;

import com.sun.javafx.binding.ObjectConstant;

import banjo.eval.Fail;
import banjo.eval.FailWithException;
import banjo.event.PastEvent;
import banjo.expr.util.ListUtil;
import banjo.value.FunctionTrait;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.data.Either;
import fj.data.List;
import javafx.beans.value.ObservableValue;

/**
 * Represents a reference to an actual java method, which can be called
 * to invoke the java method.
 */
public class JavaMethodValue extends FunctionTrait implements Value {
	public final Object target;
	public final Method[] methods;
	public JavaMethodValue(Object target, Method[] methods) {
        super();
        this.target = target;
        this.methods = methods;
    }

	@Override
	public Reaction<Value> react(PastEvent event) {
		return Reaction.of(this); // No stepping this one, it's all plain java objects
	}
	
	@Override
	public boolean isReactive() {
		return false;
	}
	
	@Override
	public ObservableValue<Value> toObservableValue() {
		return ObjectConstant.valueOf(this);
	}
	
	@Override
	public Value call(Value recurse, Value prevImpl, List<Value> arguments) {
		return new JavaMethodCall(target, methods, arguments);
	}

	@Override
	public String toStringFallback() {
	    return "java method caller(["+ListUtil.insertCommas(Arrays.asList(methods))+"])";
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
	        	return new FailWithException(e);
	        }
		}
		return new FailWithException(new NoSuchMethodError("Failed to find a compatible method for args "+arguments+" among "+ListUtil.insertCommas(Arrays.asList(methods))));
	}
}