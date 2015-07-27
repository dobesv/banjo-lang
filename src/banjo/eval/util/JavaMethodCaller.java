package banjo.eval.util;

import java.lang.reflect.Constructor;
import java.lang.reflect.Executable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.function.Function;

import banjo.eval.Fail;
import banjo.event.Event;
import banjo.expr.util.ListUtil;
import banjo.value.FunctionTrait;
import banjo.value.JavaObjectValue;
import banjo.value.Reaction;
import banjo.value.Value;
import banjo.value.meta.WrapperValue;
import fj.P2;
import fj.data.Either;
import fj.data.List;

public class JavaMethodCaller extends FunctionTrait implements Value {
	public final Object target;
	public final Method[] methods;
	public JavaMethodCaller(Object target, Method[] methods) {
        super();
        this.target = target;
        this.methods = methods;
    }

	public JavaMethodCaller(Object target, String slotName) {
		this(target, JavaObjectValue.instanceMethodsWithName(target.getClass(), slotName));
	}
	
	@Override
	public Reaction<Value> react(Event event) {
		return Reaction.none(this); // No stepping this one, it's all plain java objects
	}
	
	@Override
	public boolean isReactive() {
		return false;
	}
	
	@Override
	public Value call(Value recurse, Value prevImpl, List<Value> arguments) {
		return new JavaMethodCallResult(target, methods, arguments);
	}

	@Override
	public String toStringFallback() {
	    return "java method caller(["+ListUtil.insertCommas(Arrays.asList(methods))+"])";
	}

	public static Value callJavaMethod(Object target, Executable[] methods, List<Value> arguments, Function<Object,Value> valueFactory) {
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
						valueFactory.apply(result);
	        } catch (InvocationTargetException | InstantiationException | IllegalAccessException | IllegalArgumentException e) {
	        	return new Fail(e);
	        }
		}
		return new Fail(new NoSuchMethodError("Failed to find a compatible method for args "+arguments+" among "+ListUtil.insertCommas(Arrays.asList(methods))));
	}
}