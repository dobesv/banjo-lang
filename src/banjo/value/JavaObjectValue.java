package banjo.value;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.concurrent.Callable;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

import com.sun.javafx.binding.ObjectConstant;

import banjo.eval.ArgumentNotSupplied;
import banjo.eval.Fail;
import banjo.eval.NotCallable;
import banjo.eval.SlotNotFound;
import banjo.eval.util.JavaMethodCall;
import banjo.eval.util.JavaMethodValue;
import banjo.eval.util.Selector;
import banjo.eval.util.SlotName;
import banjo.event.PastEvent;
import banjo.expr.source.Operator;
import fj.data.Either;
import fj.data.List;
import fj.data.Stream;
import javafx.beans.value.ObservableValue;

public class JavaObjectValue implements Value {

	public static final class IsInstanceOf extends BaseInertValue implements Value, Function<Value, Value> {
	    private final Class<?> clazz;

	    public IsInstanceOf(Class<?> clazz) {
		    this.clazz = clazz;
	    }

	    public Value apply(Value x) {
	    	Either<Object,Fail> res = x.convertToJava(Object.class);
	    	boolean isInstance = res.isLeft() && clazz.isInstance(res.left().value());
	    	return Value.fromJava(isInstance);
	    }

	    public Value call(List<Value> args) {
	    	if(args.isEmpty()) return new ArgumentNotSupplied("Missing argument to isInstance()");
	    	return apply(args.head());
	    }

	    @Override
	    public Value call(Value recurse, Value baseImpl, List<Value> arguments) {
	        return call(arguments);
	    }
    }

	public final Object object;

	public JavaObjectValue(Object x) {
		this.object = x;
		if(x instanceof Value)
			throw new IllegalArgumentException("Should only use this to wrap non-Value java objects");
    }

    @Override
	public <T> Either<T, Fail> convertToJava(Class<T> clazz) {
		clazz = primitiveClassToNormalClass(clazz);
		try {
			return Either.left(clazz.cast(object));
		} catch(ClassCastException cce) {
			return Either.right(new Fail("Cannot convert object "+object+" of "+object.getClass()+" to "+clazz, cce));
		}
	}

	@SuppressWarnings("unchecked")
    public static <T> Class<T> primitiveClassToNormalClass(Class<T> clazz) {
	    if(clazz.isPrimitive()) {
			clazz = (Class<T>) (
					clazz == Boolean.TYPE ? Boolean.class :
					clazz == Character.TYPE ? Character.class :
					clazz == Byte.TYPE ? Byte.class :
					clazz == Short.TYPE ? Short.class :
					clazz == Integer.TYPE ? Integer.class :
					clazz == Long.TYPE ? Long.class :
					clazz == Float.TYPE ? Float.class :
					clazz == Double.TYPE ? Double.class :
					clazz == Void.TYPE ? Void.class :
					Object.class
			);
		}
	    return clazz;
    }

	@Override
	public Value slot(Value self, String name, Value fallback) {
		return readJavaObjectSlot(self, fallback, name, object);
	}

	@Override
	public Value call(List<Value> arguments) {
		return callJavaObject(this, null, arguments, object);
	}

	@Override
	public Value call(Value recurse, Value baseImpl, List<Value> arguments) {
		return callJavaObject(recurse, baseImpl, arguments, object);
	}

	@Override
	public Value slot(String name) {
	    return readJavaObjectSlot(this, null, name, object);
	}

	public static String methodSlotName(Method method) {
		SlotName slotAnn = method.getAnnotation(SlotName.class);
		if(slotAnn != null)
			return slotAnn.value();
		return method.getName();
	}
	public static Value readJavaObjectSlot(Value self, Value baseValue, String name, Object obj) {
	    if(name.equals("java string") && obj instanceof String) {
			return Value.fromJava(obj);
		} else if(obj == null || obj instanceof Throwable) {
			return baseValue != null ? baseValue : new SlotNotFound(name, self);
		} else {

			try {
				final Class<? extends Object> objClass = obj.getClass();
				final boolean isClass = obj instanceof Class;
				Method[] methods = isClass ?
						classMethodsWithName((Class<?>)obj, name) :
						instanceMethodsWithName(objClass, name);
				// Automatically call getters
				if(methods.length == 1 && methods[0].getParameterCount() == 0) {
					return JavaMethodValue.callJavaMethod(obj, methods, List.nil());
				}
				if(methods.length > 0) {
					return new JavaMethodValue(obj, methods);
				} else {
					// Special support for Boolean, just implement "&&", "||", and "?:" so java booleans
					// can be used in simple logical operations without conversion.
					if(obj instanceof Boolean) {
						if(((Boolean)obj).booleanValue()) {
							if(name.equals("if")) {
								return new Selector("true");
							} else if(name.equals(Operator.LOGICAL_AND.methodName)) {
	    						// Return the object that was provided
								return Value.IDENTITY_FUNCTION;
	    					} else if(name.equals(Operator.LOGICAL_OR.methodName) || name.equals(Operator.FALLBACK.methodName)){
	    						// Return the original object (Boolean.TRUE)
	    						return Value.function(x -> self);
	    					}
						} else {
							if(name.equals("if")) {
								return new Selector("false");
							} else if(name.equals(Operator.LOGICAL_AND.methodName)) {
	    						// Return the original object (Boolean.FALSE)
								return Value.function(x -> self);
	    					} else if(name.equals(Operator.LOGICAL_OR.methodName) || name.equals(Operator.FALLBACK.methodName)) {
	    						// Return the object that was provided
	    						return Value.IDENTITY_FUNCTION;
	    					}
						}
					} else if(isClass) {
						final Class<?> clazz = (Class<?>)obj;
						if(name.equals(Operator.MEMBER_OF.methodName)) {
							return new IsInstanceOf(clazz);
						} else {
							for(Class<?> innerClass : clazz.getDeclaredClasses()) {
								final SlotName slotAnn = innerClass.getAnnotation(SlotName.class);
								final String slotName = slotAnn != null ? slotAnn.value() : innerClass.getSimpleName();
								if(name.equals(slotName)) {
									return Value.fromClass(innerClass);
								}
							}
						}
					}
					if("label".equals(name)) {
						return Value.fromJava(obj.toString());
					} else if(baseValue != null) {
						return baseValue;
					} else {
						return new SlotNotFound(name, self);
					}
				}

	        } catch (IllegalArgumentException | SecurityException e) {
	        	return new Fail(e);
	        }
		}
    }
	public static Stream<Method> staticMethodsWithName(Class<?> clazz,
            String name) {
	    return Stream.<Method>stream(clazz.getDeclaredMethods())
	    		.filter(m ->
	    		name.equals(methodSlotName(m))
	    		&& (m.getModifiers() & (Modifier.STATIC | Modifier.PUBLIC)) == (Modifier.STATIC | Modifier.PUBLIC));
    }

	public static Stream<Method> instanceMethodsWithName_(
            final Class<? extends Object> objClass, String name) {
	    return Stream.<Method>stream(objClass.getMethods())
	    		.filter(m -> name.equals(methodSlotName(m)) && (m.getModifiers() & (Modifier.STATIC | Modifier.PUBLIC)) == Modifier.PUBLIC);
    }

	public static Method[] instanceMethodsWithName(
            final Class<? extends Object> objClass, String name) {
	    Method[] methods = instanceMethodsWithName_(objClass, name)
	    		.array(Method[].class);
	    return methods;
    }

	public static Method[] classMethodsWithName(
            final Class<? extends Object> objClass, String name) {
	    Method[] methods = instanceMethodsWithName_(Class.class, name)
	    		.append(staticMethodsWithName(objClass, name))
	    		.array(Method[].class);
	    return methods;
    }

	public static Value maybeWrap(Object javaObject) {
		if(javaObject instanceof Value) {
			return (Value)javaObject;
		}
		return Value.fromJava(javaObject);
	}
	
	public static Value callJavaObject(Value recurseFunction,
			Value baseFunction, List<Value> args, Object f) {
		try {
	        if(f instanceof Callable) {
	        	try {
	                return maybeWrap(((Callable<?>)f).call());
                } catch (Exception e) {
                	return new Fail(e);
                }
	        }
	        if(f instanceof Function) {
	        	return new JavaMethodCall(f, instanceMethodsWithName(f.getClass(), "apply"), args.take(1));
	        }
	        if(f instanceof BiFunction) {
	        	return new JavaMethodCall(f, instanceMethodsWithName(f.getClass(), "apply"), args.take(2));
	        }
	        if(f instanceof Supplier) {
	        	return maybeWrap(((Supplier<?>)f).get());
	        }
	        if(f instanceof Class) {
	        	if(args.isEmpty())
	        		return maybeWrap(((Class<?>)f).newInstance());
	        	return new JavaMethodCall(null, ((Class<?>)f).getConstructors(), args);
	        }
        } catch (IllegalAccessException
                | SecurityException | InstantiationException e) {
        	return new Fail(e);
        }
		if(baseFunction == null)
			return new NotCallable(f);
		return baseFunction.call(recurseFunction, null, args);
    }

	@Override
	public String toString() {
		return object.toString();
	}
	
	@Override
	public Reaction<Value> react(PastEvent event) {
		if(object instanceof Reactive) {
			Reactive<?> a = (Reactive<?>)object;
			return Reaction.to(a, event).map(this::update);
		}
		return Reaction.of(this);
	}
	
	@Override
	public boolean isReactive() {
		return (object instanceof Reactive) && ((Reactive<?>)object).isReactive();
	}
	
	public Value update(Object newObject) {
		if(newObject == this.object)
			return this;
		return new JavaObjectValue(newObject);
	}

	@Override
	public ObservableValue<Value> toObservableValue() {
		return ObjectConstant.valueOf(this);
	}
}
