package banjo.eval.util;

import interceptors.ArgInterceptor;
import interceptors.CallInterceptor;
import interceptors.CallResultInterceptor;
import interceptors.SlotInterceptor;

import java.lang.reflect.Constructor;
import java.lang.reflect.Executable;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.concurrent.Callable;
import java.util.function.Function;
import java.util.function.Supplier;

import banjo.dom.source.Operator;
import banjo.eval.Fail;
import banjo.eval.NotCallable;
import banjo.eval.SlotNotFound;
import banjo.eval.Value;
import fj.data.List;

public class JavaRuntimeSupport {

	public static Error fail(String message) {
		return new Fail(message);
	}
	/**
	 * Implement some kinds of automatic conversions.
	 *
	 * @param clazz
	 * @param banjoValue
	 * @return
	 */
	@SuppressWarnings("unchecked")
    public static <T> T convertToJava(Class<T> clazz, Object banjoValue) {
		Object v = force(banjoValue);
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
		try {
			return clazz.cast(v);
		} catch(ClassCastException cce) {
			if(!isDefined(v)) {
				if(v instanceof Error) throw (Error)v;
				throw new IllegalStateException("Cannot convert undefined value to "+clazz, v instanceof Throwable ? (Throwable)v : null);
			}

			// Needs some kind of conversio
			Object conversion = readSlot(banjoValue, banjoValue, null, "conversions");
			for(String s : clazz.getName().split("\\.")) {
				if(!isDefined(conversion)) {
					if(conversion instanceof Error) throw (Error)conversion;
					throw new IllegalStateException("No conversion defined from " + (clazz == String.class ? v.getClass().getName() : v.toString()) + " to " +clazz, conversion instanceof Throwable ? (Throwable)conversion : null);
				}
				conversion = force(readSlot(conversion, conversion, null, s));
			}
			if(!isDefined(conversion)) {
				if(conversion instanceof Error) throw (Error)conversion;
				throw new IllegalStateException("No conversion defined from " + (clazz == String.class ? v.getClass().getName() : v.toString()) + " to " +clazz, conversion instanceof Throwable ? (Throwable)conversion : null);
			}
			return clazz.cast(conversion);
		}
	}

	public static Object callJavaMethod(Object target, Executable[] methods, List<Object> arguments) {
		Object[] argumentArray = arguments.array(Object[].class);
		int argumentCount = argumentArray.length;
		methodLoop: for(Executable method : methods) {
			final int parameterCount = method.getParameterCount();
			if(argumentCount < parameterCount)
				continue;

			Class<?>[] paramTypes = method.getParameterTypes();
			Object[] convertedParams = new Object[parameterCount];
			for(int i = 0; i < parameterCount; i++) {
				try {
					convertedParams[i] = convertToJava(paramTypes[i], argumentArray[i]);
				} catch(Exception e) {
					// Failed to convert
					continue methodLoop;
				}
			}

			try {
				method.setAccessible(true);
				Object result =
				 (method instanceof Constructor) ?
					((Constructor<?>)method).newInstance(convertedParams) :
					((Method)method).invoke(target, convertedParams);
				return result;
            } catch (Exception e) {
            	return e;
            }
		}
		return new NoSuchMethodError("Failed to find a compatible method: "+target+"."+methods[0].getName());
	}
	public static Object readSlot(Object obj, String name) {
		return readSlot(obj, obj, null, name);
	}
	public static Object readTrueSlot(Object obj) {
		return readSlot(obj, "true");
	}
	public static Object readFalseSlot(Object obj) {
		return readSlot(obj, "false");
	}
	public static Object readSlot(Object maybeLazyObj, Object self, Object baseValue, String name) {
		Object obj = force(maybeLazyObj);
		if(obj instanceof Value) {
			return ((Value)obj).slot(self, name, baseValue);
		} else if(name.equals("label")) {
			// If it's not a Value subclass, just implement the "label" slot as toString()
			return String.valueOf(obj);
		}
		if(obj == null) {
			if(baseValue != null)
				return baseValue;
			return new SlotNotFound(name, self);
		}
		if(obj instanceof Throwable) {
			// Can't read slots from undefined values (represented as java exceptions)
			return obj;
		}

		try {
			Method[] methods = Arrays.asList(obj.getClass().getMethods())
					.stream()
					.filter(m -> m.getName().equals(name) && (m.getModifiers() & (Modifier.STATIC | Modifier.PUBLIC)) == Modifier.PUBLIC)
					.toArray(Method[]::new);
			if((obj instanceof Class) && methods.length == 0) {
				methods = Arrays.asList(((Class)obj).getDeclaredMethods())
						.stream()
						.filter(m -> m.getName().equals(name) && (m.getModifiers() & (Modifier.STATIC | Modifier.PUBLIC)) == (Modifier.STATIC | Modifier.PUBLIC))
						.toArray(Method[]::new);
			}
			if(methods.length > 0) {
				return new OverloadedJavaMethodCaller(obj, methods);
			} else {
				try {
					Field field = obj.getClass().getField(name);
					return field.get(obj);
				} catch(Throwable t) {
					// Special support for Boolean, just implement "&&", "||", and "?:" so java booleans
					// can be used in simple logical operations without conversion.
					if(obj instanceof Boolean) {
						if(((Boolean)obj).booleanValue()) {
							if(name.equals("if")) {
								return (Function<?,?>)(JavaRuntimeSupport::readTrueSlot);
							}
	    					if(name.equals(Operator.LOGICAL_AND.methodName)) {
	    						// Return the object that was provided
	    						return Function.identity();
	    					}
	    					if(name.equals(Operator.LOGICAL_OR.methodName) || name.equals(Operator.FALLBACK.methodName)){
	    						// Return the original object (Boolean.TRUE)
	    						return (Function<?,?>)(x -> obj);
	    					}
						} else {
							if(name.equals("if")) {
								return (Function<?,?>)(JavaRuntimeSupport::readFalseSlot);
							}
	    					if(name.equals(Operator.LOGICAL_AND.methodName)) {
	    						// Return the original object (Boolean.FALSE)
	    						return (Function<?,?>)(x -> obj);
	    					}
	    					if(name.equals(Operator.LOGICAL_OR.methodName) || name.equals(Operator.FALLBACK.methodName)) {
	    						// Return the object that was provided
	    						return Function.identity();
	    					}
						}
					} else if(obj instanceof Class) {
						final Class<?> clazz = (Class<?>)obj;
						if(name.equals(Operator.MEMBER_OF.methodName)) {
							return (Function<Object,Boolean>)clazz::isInstance;
						}

						try {
							return clazz.getField(name).get(null);
						} catch(Throwable tt) {
						}

						for(Class<?> innerClass : clazz.getDeclaredClasses()) {
							if(name.equals(innerClass.getSimpleName())) {
								return innerClass;
							}
						}
					}
					if(!(obj instanceof Value) && "label".equals(name)) {
						return obj.toString();
					}
					if(baseValue == null)
						return new SlotNotFound(name, self, t);
					return baseValue;
				}
			}
        } catch (IllegalArgumentException | SecurityException e) {
        	return e;
        }
	}

	/**
	 * Call a callable target with the given arguments.
	 * @param recurseFunction TODO
	 * @param baseFunction TODO
	 */
	@SuppressWarnings("unchecked")
    public static Object call(Object callee, Object recurseFunction, Object baseFunction, List<Object> args) {
		Object f = force(callee);
		if(f instanceof Value) {
			return ((Value)f).call(recurseFunction, baseFunction, args);
		}
		if(!isDefined(f))
			return f;

		try {
	        if(f instanceof Callable) {
	        	try {
	                return ((Callable<Object>)f).call();
                } catch (Exception e) {
                	return new Fail(e);
                }
	        }
	        if(f instanceof Function) {
	        	return ((Function<Object,Object>)f).apply(force(args.toOption().toNull()));
	        }
	        if(f instanceof Supplier) {
	        	return ((Supplier<Object>)f).get();
	        }
	        if(f instanceof Class) {
	        	if(args.isEmpty())
	        		return ((Class<?>)f).newInstance();
	        	return callJavaMethod(null, ((Class<?>)f).getConstructors(), args);
	        }
        } catch (IllegalAccessException
                | SecurityException | InstantiationException e) {
        	return new Fail(e);
        }
		if(baseFunction == null)
			return new NotCallable(f);
		return call(baseFunction, recurseFunction, null, args);
	}

	/**
	 * Simpler wrapper for a top-level call without any extension information.
	 */
	public static Object call(Object f, List<Object> args) {
		return call(f, f, null, args);
	}

	/**
	 * Call a method on the given target with the given arguments.  In some cases
	 * this will be more efficient then doing call(readSlot(obj, name), args)
	 */
	public static Object callMethod(Object obj, String name, List<Object> args) {
	    return callMethod(obj, name, obj, null, args);
	}

	/**
	 * Call a method on the given target with the given arguments.  In some cases
	 * this will be more efficient then doing call(readSlot(obj, name), args)
	 *
	 * @param target Object to lookup the method implementation from
	 * @param name Name of the method to call
	 * @param targetObject Object to lookup slots from for self-references or self-method-calls
	 * @param fallback If the method is not implemented, this lazily supplies a substitute return value (by calling a base object method or returning an error)
	 * @param args Method arguments to pass
	 */
	public static Object callMethod(Object target, String name, Object targetObject, Object fallback, List<Object> args) {
		Object realTarget = force(target);
		if(realTarget instanceof Value)
			return ((Value)realTarget).callMethod(name, targetObject, fallback, args);
		final Object f = readSlot(realTarget, realTarget, null, name);
		if(fallback != null && !isDefined(f)) {
			return fallback;
		}
		return call(f, f, null, args);
	}

	/**
	 * Shortcut for calling a unary method.
	 */
	public static Object callMethod(Object obj, String name, Object arg) {
		return callMethod(obj, name, obj, null, List.single(arg));
	}

	/**
	 * If a value is lazy, evaluate it to yield the "final" object.
	 */
	public static Object force(Object obj) {
		while(obj instanceof Supplier) {
			obj = ((Supplier<?>)obj).get();
		}
		return obj;
	}

	/**
	 * Return true if the value is not null
	 * @param obj
	 * @return
	 */
	public static boolean isDefined(Object obj) {
		return obj != null && !(force(obj) instanceof Throwable);
	}

	public static boolean isTruthy(Object obj) {
		if(obj instanceof Boolean) {
			return ((Boolean)obj).booleanValue();
		}
		try {
			final Object callResult = callMethod(obj, Operator.LOGICAL_AND.methodName, Boolean.TRUE);
			final Object val = force(callResult);
			return Boolean.TRUE == val;
		} catch(Exception e) {
			return false; // Failure is not truthy
		}
	}

	public static boolean isTrue(Object obj) {
		return convertToJava(Boolean.class, obj) == Boolean.TRUE;
	}

	public static Object applyBoolean(boolean a, Object ifTrue, Object ifFalse) {
		return a ? ifTrue : ifFalse;
	}

	/**
	 * Implement function composition.  The first function is called with with the
	 * arguments to this function.  The result of calling first is passed to second.
	 * The result of second is the result of the function.
	 */
	public static Object composeFunctions(Object first, Object second) {
		return new CallResultInterceptor(second, first);
	}

	/**
	 * Function result interceptor.  Equivalent to function composition.
	 */
	public static Object functionResultInterceptor(Object interceptor, Object target) {
		return new CallResultInterceptor(interceptor, target);
	}

	/**
	 * Slot interceptor.  Passes each slot value through a function before returning it.
	 */
	public static Object slotInterceptor(Object interceptor, Object target) {
		return new SlotInterceptor(interceptor, target);
	}

	/**
	 * Function argument interceptor - passes each function argument through a function
	 * before passing it into the given object.
	 */
	public static Object argInterceptor(Object interceptor, Object target) {
		return new ArgInterceptor(interceptor, target);
	}

	/**
	 * Call interceptor - gets function arguments as a tuple to use as it
	 * pleases.
	 */
	public static Object callInterceptor(Object interceptor, Object target) {
		return new CallInterceptor(interceptor, target);
	}

	private static final class LazySlotValue implements Supplier<Object> {
	    public final Object baseObject;
	    public final String name;

		public LazySlotValue(Object baseObject, String name) {
	    	this.baseObject = baseObject;
	    	this.name = name;
        }

		@Override
	    public Object get() {
	    	return readSlot(baseObject, baseObject, null, name);
	    }
    }

	public static class Integers {
		public static int sum(int a, int b) { return a + b; }
		public static int difference(int a, int b) { return a - b; }
		public static int product(int a, int b) { return a * b; }
		public static int quotient(int a, int b) { return a / b; }
		public static int neg(int a) { return -a; }
		public static int abs(int a) { return java.lang.Math.abs(a); }
		public static boolean eq(int a, int b) { return a == b; }
		public static boolean ne(int a, int b) { return a != b; }
		public static boolean gt(int a, int b) { return a > b; }
		public static boolean ge(int a, int b) { return a >= b; }
		public static boolean lt(int a, int b) { return a < b; }
		public static boolean le(int a, int b) { return a <= b; }
		public static Object cmp(int a, int b, Object ascending, Object equal, Object descending, Object undefined) {
			return (a < b) ? ascending : (a > b) ? descending : equal;
		}
		public static Object checkSign(int a, Object negative, Object zero, Object positive) {
			return (a < 0) ? negative : (a > 0) ? positive : zero;
		}
		public static BigDecimal toBigDecimal(int a) { return BigDecimal.valueOf(a); }
		public static BigInteger toBigInteger(int a) { return BigInteger.valueOf(a); }
	}

	public static class Longs {
		public static long sum(long a, long b) { return a + b; }
		public static long difference(long a, long b) { return a - b; }
		public static long product(long a, long b) { return a * b; }
		public static long quotient(long a, long b) { return a / b; }
		public static long neg(long a) { return -a; }
		public static long abs(long a) { return java.lang.Math.abs(a); }
		public static boolean eq(long a, long b) { return a == b; }
		public static boolean ne(long a, long b) { return a != b; }
		public static boolean gt(long a, long b) { return a > b; }
		public static boolean ge(long a, long b) { return a >= b; }
		public static boolean lt(long a, long b) { return a < b; }
		public static boolean le(long a, long b) { return a <= b; }
		public static Object cmp(long a, long b, Object ascending, Object equal, Object descending, Object undefined) {
			return (a < b) ? ascending : (a > b) ? descending : equal;
		}
		public static Object checkSign(long a, Object negative, Object zero, Object positive) {
			return (a < 0) ? negative : (a > 0) ? positive : zero;
		}
		public static BigDecimal toBigDecimal(long a) { return BigDecimal.valueOf(a); }
		public static BigInteger toBigInteger(long a) { return BigInteger.valueOf(a); }
	}

	public static class Floats {
		public static float sum(float a, float b) { return a + b; }
		public static float difference(float a, float b) { return a - b; }
		public static float product(float a, float b) { return a * b; }
		public static float quotient(float a, float b) { return a / b; }
		public static float neg(float a) { return -a; }
		public static float abs(float a) { return java.lang.Math.abs(a); }
		public static boolean eq(float a, float b) { return a == b; }
		public static boolean ne(float a, float b) { return a != b; }
		public static boolean gt(float a, float b) { return a > b; }
		public static boolean ge(float a, float b) { return a >= b; }
		public static boolean lt(float a, float b) { return a < b; }
		public static boolean le(float a, float b) { return a <= b; }
		public static Object cmp(float a, float b, Object ascending, Object equal, Object descending, Object undefined) {
			return (a < b) ? ascending : (a > b) ? descending : (a == b) ? equal : undefined;
		}
		public static Object checkSign(float a, Object negative, Object zero, Object positive) {
			return (a < 0) ? negative : (a > 0) ? positive : zero;
		}
		public static BigDecimal toBigDecimal(float a) { return new BigDecimal(a); }
		public static BigInteger toBigInteger(float a) { return BigInteger.valueOf((long)a); }
	}

	public static class Doubles {
		public static double sum(double a, double b) { return a + b; }
		public static double difference(double a, double b) { return a - b; }
		public static double product(double a, double b) { return a * b; }
		public static double quotient(double a, double b) { return a / b; }
		public static double neg(double a) { return -a; }
		public static double abs(double a) { return java.lang.Math.abs(a); }
		public static boolean eq(double a, double b) { return a == b; }
		public static boolean ne(double a, double b) { return a != b; }
		public static boolean gt(double a, double b) { return a > b; }
		public static boolean ge(double a, double b) { return a >= b; }
		public static boolean lt(double a, double b) { return a < b; }
		public static boolean le(double a, double b) { return a <= b; }
		public static Object cmp(double a, double b, Object ascending, Object equal, Object descending, Object undefined) {
			return (a < b) ? ascending : (a > b) ? descending : (a == b) ? equal : undefined;
		}
		public static Object checkSign(double a, Object negative, Object zero, Object positive) {
			return (a < 0) ? negative : (a > 0) ? positive : zero;
		}
		public static BigDecimal toBigDecimal(double a) { return new BigDecimal(a); }
		public static BigInteger toBigInteger(double a) { return BigInteger.valueOf((long)a); }

	}

	public static class BigIntegers {
		public static BigInteger sum(BigInteger a, BigInteger b) { return a.add(b); }
		public static BigInteger difference(BigInteger a, BigInteger b) { return a.subtract(b); }
		public static BigInteger product(BigInteger a, BigInteger b) { return a.multiply(b); }
		public static BigInteger quotient(BigInteger a, BigInteger b) { return a.divide(b); }
		public static BigInteger neg(BigInteger a) { return a.negate(); }
		public static BigInteger abs(BigInteger a) { return a.abs(); }
		public static boolean eq(BigInteger a, BigInteger b) { return a.compareTo(b) == 0; }
		public static boolean ne(BigInteger a, BigInteger b) { return a.compareTo(b) != 0; }
		public static boolean gt(BigInteger a, BigInteger b) { return a.compareTo(b) > 0; }
		public static boolean ge(BigInteger a, BigInteger b) { return a.compareTo(b) >= 0; }
		public static boolean lt(BigInteger a, BigInteger b) { return a.compareTo(b) < 0; }
		public static boolean le(BigInteger a, BigInteger b) { return a.compareTo(b) <= 0; }
		public static Object cmp(BigInteger a, BigInteger b, Object ascending, Object equal, Object descending, Object undefined) {
			int c = a.compareTo(b);
			return (c < 0) ? ascending : (c > 0) ? descending : equal;
		}
		public static Object checkSign(BigInteger a, Object negative, Object zero, Object positive) {
			return Integers.checkSign(a.signum(), negative, zero, positive);
		}
		public static BigDecimal toBigDecimal(BigInteger a) { return new BigDecimal(a); }
		public static BigInteger toBigInteger(BigInteger a) { return a; }

	}

	public static class BigDecimals {
		public static BigDecimal sum(BigDecimal a, BigDecimal b) { return a.add(b); }
		public static BigDecimal difference(BigDecimal a, BigDecimal b) { return a.subtract(b); }
		public static BigDecimal product(BigDecimal a, BigDecimal b) { return a.multiply(b); }
		public static BigDecimal quotient(BigDecimal a, BigDecimal b) { return a.divide(b); }
		public static BigDecimal neg(BigDecimal a) { return a.negate(); }
		public static BigDecimal abs(BigDecimal a) { return a.abs(); }
		public static boolean eq(BigDecimal a, BigDecimal b) { return a.compareTo(b) == 0; }
		public static boolean ne(BigDecimal a, BigDecimal b) { return a.compareTo(b) != 0; }
		public static boolean gt(BigDecimal a, BigDecimal b) { return a.compareTo(b) > 0; }
		public static boolean ge(BigDecimal a, BigDecimal b) { return a.compareTo(b) >= 0; }
		public static boolean lt(BigDecimal a, BigDecimal b) { return a.compareTo(b) < 0; }
		public static boolean le(BigDecimal a, BigDecimal b) { return a.compareTo(b) <= 0; }
		public static Object cmp(BigDecimal a, BigDecimal b, Object ascending, Object equal, Object descending, Object undefined) {
			int c = a.compareTo(b);
			return (c < 0) ? ascending : (c > 0) ? descending : equal;
		}
		public static Object checkSign(BigDecimal a, Object negative, Object zero, Object positive) {
			return Integers.checkSign(a.signum(), negative, zero, positive);
		}
		public static BigDecimal toBigDecimal(BigDecimal a) { return a; }
		public static BigInteger toBigInteger(BigDecimal a) { return a.toBigInteger(); }
	}

	public static class Strings {
		public static String concat(Object a, Object b) { return String.valueOf(a) + String.valueOf(b); }
		public static boolean eq(String a, String b) { return a.equals(b); }
		public static boolean ne(String a, String b) { return !a.equals(b); }
		public static boolean gt(String a, String b) { return a.compareTo(b) < 0; }
		public static boolean ge(String a, String b) { return a.compareTo(b) <= 0; }
		public static boolean lt(String a, String b) { return a.compareTo(b) > 0; }
		public static boolean le(String a, String b) { return a.compareTo(b) >= 0; }
		public static Object cmp(String a, String b, Object ascending, Object equal, Object descending, Object undefined) {
			int cmp = a.compareTo(b);
			return cmp < 0 ? ascending : cmp > 0 ? descending : equal;
		}
	}

	public static final Boolean TRUE = Boolean.TRUE;
	public static final Boolean FALSE = Boolean.FALSE;

	public static final ThreadLocal<List<Supplier<StackTraceElement>>> stack = ThreadLocal.<List<Supplier<StackTraceElement>>>withInitial(List::nil);

	public static PackageValue getJavaPackage(String name) {
		return PackageValue.forName(name);
	}

	public static final PackageValue javaPackage = PackageValue.forName("java");
	public static final PackageValue banjoPackage = PackageValue.forName("banjo");

	public boolean isByte(Object x) { return x instanceof Byte; }
	public boolean isShort(Object x) { return x instanceof Short; }
	public boolean isInteger(Object x) { return x instanceof Integer; }
	public boolean isLong(Object x) { return x instanceof Long; }
	public boolean isFloat(Object x) { return x instanceof Float; }
	public boolean isDouble(Object x) { return x instanceof Double; }
	public boolean isBigInteger(Object x) { return x instanceof BigInteger; }
	public boolean isBigDecimal(Object x) { return x instanceof BigDecimal; }

}
