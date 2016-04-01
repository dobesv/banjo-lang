package banjo.value;

import java.util.HashMap;
import java.util.function.Function;

import com.sun.javafx.binding.ObjectConstant;

import banjo.eval.ClosedObject;
import banjo.eval.ExtendedObject;
import banjo.eval.Fail;
import banjo.eval.FailWithMessage;
import banjo.eval.NotCallable;
import banjo.eval.SlotNotFound;
import banjo.expr.source.Operator;
import banjo.expr.util.SourceFileRange;
import banjo.value.meta.SlotMemoizer;
import fj.data.Either;
import fj.data.List;
import fj.data.Set;
import javafx.beans.value.ObservableValue;

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
        return new NotCallable(recurse, SourceFileRange.EMPTY_SET);
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
	 * @param ranges TODO
	 * @param fallback Value to return if the slot is not found
	 */
	public default Value slot(Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
		if(fallback != null)
			return fallback;
        return new SlotNotFound(name, ranges, self);
	}

	/**
     * Get the value of a slot.
     *
     * Returns an instance of SlotNotFound if the slot is not defined.
     * 
     * @param ranges
     *            Source file ranges to blame if the slot is undefined
     */
	public default Value slot(String name, Set<SourceFileRange> ranges) {
		return slot(this, name, ranges, null);
	}

    /**
     * Lookup a slot.
     * <p>
     * If you have relevant source file ranges available, use the one that takes
     * those ranges to improve debugging.
     * 
     * @param name
     *            Name of the slot to get the value of
     * @return The value of the slot, or some <code>Fail<code> instance if the
     *         slot value is not defined for any reason
     */
    public default Value slot(String name) {
        return slot(this, name, SourceFileRange.EMPTY_SET, null);
    }

	static final Value MISSING_METHOD_PLACEHOLDER = new FailWithMessage("Missing slot for method");

	/**
     * Call a function in a slot. A subclass may provide an optimized
     * implementation in some cases. Otherwise, this just fetches the slot and
     * calls it.
     *
     * Note that in the case of a method call one optimization is already in
     * place - the given "fallback" parameter is a fallback for the entire
     * method call, not just the slot reference. So if the slot is not defined,
     * the fallback can be returned without calling it.
     *
     * @param name
     *            Name of the method to call
     * @param ranges
     *            Source file ranges of the method call, if available, otherwise
     *            an empty set
     * @param targetObject
     *            Original target object of the call if this callee is part of
     *            an extended object
     * @param fallback
     *            If the method is not implemented, this lazily supplies a
     *            return value; null if no applicable fallback
     * @param args
     *            List of arguments to pass
     * @return
     */
	public default Value callMethod(String name, Set<SourceFileRange> ranges, Value targetObject, Value fallback, List<Value> args) {
        final Value f = slot(targetObject, name, ranges, MISSING_METHOD_PLACEHOLDER).force();
	    if(fallback != null && !f.isDefined()) {
    		return fallback;
	    }
		return f.call(f, null, args);
    }

	/**
	 * Simple method call with defaults
	 */
    public default Value callMethod(String name, Set<SourceFileRange> ranges, List<Value> args) {
		return callMethod(name, ranges, this, null, args);
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
            Value conversion = slot("conversions", SourceFileRange.EMPTY_SET);
			for(String s : clazz.getName().split("\\.")) {
                conversion = conversion.slot(s, SourceFileRange.EMPTY_SET);
			}
			return conversion.convertToJava(clazz);
		}
	}

	/**
	 * Wrap a java object as a Value.  Reflection will be used
	 * to access the slots of the java object or call it.  Note
	 * that the value will NOT update in response to events.
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
			staticJavaObjectCache.put(name, v = fromJava(clazz));
		}
		return v;
    }

	/**
	 *
	 * @return true if this value is "truthy", as in "this && true == true".
	 */
	public default boolean isTruthy() {
		try {
			Value fallbackValue = fromJava(Boolean.FALSE); // If no such method, it's not truthy
            final Value callResult =
                callMethod(Operator.LOGICAL_AND.methodName, SourceFileRange.currentJavaThreadLoc(), this, fallbackValue, List.single(fromJava(Boolean.TRUE)));
			return callResult.convertToJava(Boolean.class).either(x -> x.booleanValue(), x -> false); // Doesn't return a boolean? not truthy
		} catch(Exception e) {
			return false; // Failure is not truthy
		}
	}

	/**
     * Default implementation of toString(). Sadly we cannot call this method
     * "toString" here because we are an interface and interfaces cannot provide
     * a default toString().
     */
	public default String javaLabel() {
	    try {
            return slot("label", SourceFileRange.currentJavaThreadLoc()).convertToJava(String.class).either(
	    			s -> s,
	    			f -> toStringFallback()
	    			);
	    } catch(Throwable t) {
            t.printStackTrace();
	    	return toStringFallback();
	    }
	}

	public default String toStringFallback() {
	    return "<"+getClass().getSimpleName()+"(Value)>";
    }

    public default String stackTraceElementString() {
        return this.toString();
    }

	public default Value extendedWith(Value extension) {
		return new ExtendedObject(this, extension);
	}

	public default <T> T readAndConvertSlot(String slotName, Class<T> clazz, Function<Fail,T> onFailure) {
        Value slotValue = this.slot(slotName, SourceFileRange.currentJavaThreadLoc());
		if(!slotValue.isDefined()) {
			Fail err = Either.reduce(slotValue.convertToJava(Fail.class));
			return onFailure.apply(err);
		}
		Either<T, Fail> conversion = slotValue.convertToJava(clazz);
		if(conversion.isLeft()) {
			return conversion.left().value();
		}
		return onFailure.apply(conversion.right().value());
	}
	
    @Override
    public default ObservableValue<Value> toObservableValue() {
	    // Inert by default
        return ObjectConstant.valueOf(this);
	}

    public default ClosedObject closed() {
        return new ClosedObject(this);
    }

}