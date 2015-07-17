package banjo.value;

import java.util.HashMap;
import java.util.function.Function;
import java.util.function.Supplier;

import banjo.eval.Fail;
import banjo.eval.NotCallable;
import banjo.eval.SlotNotFound;
import banjo.eval.util.MemoizingSupplier;
import banjo.expr.source.Operator;
import banjo.value.meta.SlotMemoizer;
import fj.data.Either;
import fj.data.List;

public interface Value extends Reactive<Value> {
	public static final Value IDENTITY_FUNCTION = function(Function.identity());

	/**
	 * If this is callable, perform the call and return the result.  Otherwise return Undefined.
	 *
	 * @param recurse A reference to the function being called, at the top level; if the function calls itself
	 * recursively it should call that function.  This may be a different function from the one passed if
	 * the function was an extension and it is calling up to its base.  If the base calls back to itself
	 * it should call the extended version.
	 * @param baseImpl If this object is an extension of another object, the other object is provided as a
	 * base implementation that this function can call.  If this object isn't callable, then the base implementation
	 * is used as a fallback.
	 */
	public default Value call(Value recurse, Value baseImpl, List<Value> arguments) {
		if(baseImpl != null) {
			return baseImpl.call(recurse, null, arguments);
		}
		return new NotCallable(recurse);
	}

	/**
	 * Simple call with default recurse (the same object) and
	 * fallback (none).
	 */
	public default Value call(List<Value> arguments) {
		return call(this, null, arguments);
	}
	
	public default Value call1(Value v) {
		return call(List.single(v));
	}
	
	/**
	 * Fetch the named slot.  If the object has no such slot, returns the baseSlotValue if provided, otherwise
	 * an error.
	 *
	 * @param self A reference to the object we are trying to get the slot from.  If the object is an extension
	 * then this may not be equal to "this".
	 * @param fallback Value to return if the slot is not found
	 */
	public default Value slot(Value self, String name, Value fallback) {
		if(fallback != null)
			return fallback;
		return new SlotNotFound(name, self);
	}

	/**
	 * Get the value of a slot.
	 *
	 * Returns an instance of SlotNotFound if the slot is not defined.
	 */
	public default Value slot(String name) {
		return slot(this, name, null);
	}

	static final Value MISSING_METHOD_PLACEHOLDER = new Fail("Missing slot for method");

	/**
	 * Call a function in a slot.  A subclass may provide an optimized
	 * implementation in some cases.  Otherwise, this just fetches the
	 * slot and calls it.
	 *
	 * Note that in the case of a method call one optimization is already
	 * in place - the given "fallback" parameter is a fallback for the
	 * entire method call, not just the slot reference.  So if the slot
	 * is not defined, the fallback can be returned without calling it.
	 *
	 * @param name Name of the method to call
	 * @param targetObject Original target object of the call if this callee is part of an extended object
	 * @param fallback If the method is not implemented, this lazily
	 *     supplies a return value; null if no applicable fallback
	 * @param args List of arguments to pass
	 * @return
	 */
	public default Value callMethod(String name, Value targetObject, Value fallback, List<Value> args) {
	    final Value f = slot(targetObject, name, MISSING_METHOD_PLACEHOLDER).force();
	    if(fallback != null && !f.isDefined()) {
    		return fallback;
	    }
		return f.call(f, null, args);
    }

	/**
	 * Simple method call with defaults
	 */
	public default Value callMethod(String name, List<Value> args) {
		return callMethod(name, this, null, args);
	}

	/**
	 * If this value is lazy, calculate the "actual" value and return it.  This
	 * may be useful for checking the class of the object or certain kinds of
	 * optimization.  In general lazy values automatically "force" when you try
	 * to call them, access their slots, convert them to java, or call their methods.
	 */
	public default Value force() {
		return this;
	}

	/**
	 * Return false if this is an "undefined" value - i.e. someone
	 * has tried to access an undefined slot or called the fail
	 * function or otherwise caused some kind of error.
	 */
	public default boolean isDefined() {
		return true;
	}

	/**
	 * Return an instance of the given java class which is isomorphic to this value.
	 *
	 * The conversion should allow any loss of precision, generally this is only used
	 * to get a java equivalent of an object, not to encode/decode/convert/parse a
	 * value.
	 */
	public default <T> Either<T, Fail> convertToJava(Class<T> clazz) {
		try {
			// If this object already implements or extends the class, we're good to go!
			return Either.left(clazz.cast(this));
		} catch(ClassCastException cce) {
			clazz = JavaObjectValue.primitiveClassToNormalClass(clazz);
			Value conversion = slot("conversions");
			for(String s : clazz.getName().split("\\.")) {
				conversion = conversion.slot(s);
			}
			return conversion.convertToJava(clazz);
		}
	}

	/**
	 * Wrap a java object as a Value.  Reflection will be used
	 * to access the slots of the java object or call it.
	 */
	public static Value fromJava(Object x) {
		return new SlotMemoizer(new JavaObjectValue(x));
	}

	/**
	 * Wrap a java Function instance as a Value.  A bit more efficient
	 * than using reflection
	 */
	public static Value function(Function<Value,Value> f) {
		return new FunctionValue(f);
	}

	static HashMap<String,Value> staticJavaObjectCache = new HashMap<String,Value>();


	/**
	 * When we know a java object is a class, we can use this instead of
	 * fromJava() and the result will be cached.
	 */
	public static Value fromClass(Class<?> clazz) {
		return staticJavaObject(clazz, clazz.getName());
	}

	/**
	 * Get a Value from an object that is known to be a static java object,
	 * so we can cache it.  For example classes and static class members.
	 */
	public static Value staticJavaObject(Class<?> clazz, final String name) {
	    Value v = staticJavaObjectCache.get(name);
		if(v == null) {
			staticJavaObjectCache.put(clazz.getName(), v = fromJava(clazz));
		}
		return v;
    }

	/**
	 *
	 * @return true if this value is "truthy", as in "this && true == true".
	 */
	public default boolean isTruthy() {
		try {
			final Value callResult = callMethod(Operator.LOGICAL_AND.methodName, this, fromJava(Boolean.FALSE), List.single(fromJava(Boolean.TRUE)));
			return callResult.convertToJava(Boolean.class).either(x -> x.booleanValue(), x -> false);
		} catch(Exception e) {
			return false; // Failure is not truthy
		}
	}

	/**
	 * Default implementation of toString().  Sadly we
	 * cannot call this toString() here because we are an interface.
	 */
	public default String javaLabel() {
	    try {
	    	return slot("label").convertToJava(String.class).either(
	    			s -> s,
	    			f -> toStringFallback()
	    			);
	    } catch(Throwable t) {
	    	//t.printStackTrace();
	    	return toStringFallback();
	    }
	}

	public default String toStringFallback() {
	    return "<"+getClass().getSimpleName()+"(Value)>";
    }

	/**
	 * Create a lazy value from a nullary function.
	 */
	public static Value lazy(Supplier<Value> calculation) {
		return new CalculatedValue(new MemoizingSupplier<Value>(() -> calculation.get().force()));
	}

}
