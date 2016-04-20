package banjo.value;

import java.util.function.BiFunction;
import java.util.function.Function;

import banjo.eval.ClosedObject;
import banjo.eval.ExtendedObject;
import banjo.eval.Fail;
import banjo.eval.FailWithMessage;
import banjo.eval.NotCallable;
import banjo.eval.SlotNotFound;
import banjo.expr.util.SourceFileRange;
import banjo.value.kernel.KernelBiFunctionValue;
import banjo.value.kernel.KernelBooleanValue;
import banjo.value.kernel.KernelFunctionValue;
import fj.data.Either;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;

public interface Value {
	public static final Value IDENTITY_FUNCTION = function(Function.identity());

	/**
     * Allow values to be inspected and walked without using casts.
     */
    public <T> T acceptVisitor(ValueVisitor<T> visitor);

    /**
     * If this is callable, perform the call and return the result. Otherwise
     * return Undefined.
     * @param trace TODO
     * @param recurse
     *            A reference to the function being called, at the top level; if
     *            the function calls itself recursively it should call that
     *            function. This may be a different function from the one passed
     *            if the function was an extension and it is calling up to its
     *            base. If the base calls back to itself it should call the
     *            extended version.
     * @param baseImpl
     *            If this object is an extension of another object, the other
     *            object is provided as a base implementation that this function
     *            can call. If this object isn't callable, then the base
     *            implementation is used as a fallback.
     */
	public default Value call(List<Value> trace, Value recurse, Value baseImpl, List<Value> arguments) {
		if(baseImpl != null) {
			return baseImpl.call(trace, recurse, null, arguments);
		}
        return new NotCallable(trace, recurse, SourceFileRange.EMPTY_SET);
	}

	/**
	 * Simple call with default recurse (the same object) and
	 * fallback (none).
	 * @param trace TODO
	 */
	public default Value call(List<Value> trace, List<Value> arguments) {
		return call(trace, this, null, arguments);
	}
	
	public default Value call1(List<Value> trace, Value v) {
		return call(trace, List.single(v));
	}
	
	/**
	 * Fetch the named slot.  If the object has no such slot, returns the baseSlotValue if provided, otherwise
	 * an error.
	 * @param trace TODO
	 * @param self A reference to the object we are trying to get the slot from.  If the object is an extension
	 * then this may not be equal to "this".
	 * @param ranges TODO
	 * @param fallback Value to return if the slot is not found
	 */
	public default Value slot(List<Value> trace, Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
		if(fallback != null)
			return fallback;
        return new SlotNotFound(trace, name, ranges, self);
	}

	/**
     * Get the value of a slot.
     *
     * Returns an instance of SlotNotFound if the slot is not defined.
	 * @param trace TODO
	 * @param ranges
     *            Source file ranges to blame if the slot is undefined
     */
	public default Value slot(List<Value> trace, String name, Set<SourceFileRange> ranges) {
		return slot(trace, this, name, ranges, null);
	}

    /**
     * Lookup a slot.
     * <p>
     * If you have relevant source file ranges available, use the one that takes
     * those ranges to improve debugging.
     * @param trace TODO
     * @param name
     *            Name of the slot to get the value of
     * 
     * @return The value of the slot, or some <code>Fail<code> instance if the
     *         slot value is not defined for any reason
     */
    public default Value slot(List<Value> trace, String name) {
        return slot(trace, this, name, SourceFileRange.EMPTY_SET, null);
    }

	/**
     * Call a function in a slot. A subclass may provide an optimized
     * implementation in some cases. Otherwise, this just fetches the slot and
     * calls it.
     *
     * Note that in the case of a method call one optimization is already in
     * place - the given "fallback" parameter is a fallback for the entire
     * method call, not just the slot reference. So if the slot is not defined,
     * the fallback can be returned without calling it.
	 * @param trace TODO
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
     *
     * @return
     */
	public default Value callMethod(List<Value> trace, String name, Set<SourceFileRange> ranges, Value targetObject, Value fallback, List<Value> args) {
        final Value f = slot(trace, targetObject, name, ranges, null).force(trace);
	    if(fallback != null && !f.isDefined(trace)) {
    		return fallback;
	    }
		return f.call(trace, f, null, args);
    }

	/**
	 * Simple method call with defaults
	 * @param trace TODO
	 */
    public default Value callMethod(List<Value> trace, String name, Set<SourceFileRange> ranges, List<Value> args) {
		return callMethod(trace, name, ranges, this, null, args);
	}

	/**
	 * If this value is lazy, calculate the "actual" value and return it.  This
	 * may be useful for checking the class of the object or certain kinds of
	 * optimization.  In general lazy values automatically "force" when you try
	 * to call them, access their slots, convert them to java, or call their methods.
	 * @param trace TODO
	 */
    public default Value force(List<Value> trace) {
		return this;
	}

	/**
	 * Return false if this is an "undefined" value - i.e. someone
	 * has tried to access an undefined slot or called the fail
	 * function or otherwise caused some kind of error.
	 * @param trace TODO
	 */
	public default boolean isDefined(List<Value> trace) {
		return true;
	}

	/**
	 * Return an instance of the given java class which is isomorphic to this value.
	 *
	 * The conversion should allow any loss of precision, generally this is only used
	 * to get a java equivalent of an object, not to encode/decode/convert/parse a
	 * value.
	 * @param trace TODO
	 */
	public default <T> Either<T, Fail> convertToJava(List<Value> trace, Class<T> clazz) {
        return Either.right(new FailWithMessage(trace, "Cannot convert " + this + " to instance of " + clazz));
	}

	/**
     * Wrap a java Function instance as a Value.
     */
    public static KernelFunctionValue function(Function<Value, Value> f) {
		return new KernelFunctionValue(f);
	}

    /**
     * Wrap a java BiFunction instance as a Value.
     */
    public static KernelBiFunctionValue function(BiFunction<Value, Value, Value> f) {
        return new KernelBiFunctionValue(f);
    }

	/**
     * Return true if this == true.
     */
    public default boolean isTrue(List<Value> trace) {
        Option<Boolean> b = KernelBooleanValue.extractBoolean(trace, this);
        return b.orSome(false);
	}

	/**
     * Default implementation of toString(). Sadly we cannot call this method
     * "toString" here because we are an interface and interfaces cannot provide
     * a default toString().
	 * @param trace TODO
     */
	public default String javaLabel(List<Value> trace) {
	    try {
            Set<SourceFileRange> loc = SourceFileRange.currentJavaThreadLoc();
            return slot(trace, "label", loc).slot(trace, "kernel string", loc).convertToJava(trace, String.class).either(
	    			s -> s,
	    			f -> toStringFallback(trace)
	    			);
	    } catch(Throwable t) {
            t.printStackTrace();
	    	return toStringFallback(trace);
	    }
	}

	public default String toStringFallback(List<Value> trace) {
	    return "<"+getClass().getSimpleName()+"(Value)>";
    }

    public default String stackTraceElementString() {
        return this.toString();
    }

	public default Value extendedWith(Value extension) {
		return new ExtendedObject(this, extension);
	}

	public default <T> T readAndConvertSlot(List<Value> trace, String slotName, Class<T> clazz, Function<Fail,T> onFailure) {
        Value slotValue = this.slot(trace, slotName, SourceFileRange.currentJavaThreadLoc());
		if(!slotValue.isDefined(trace)) {
			Fail err = Either.reduce(slotValue.convertToJava(trace, Fail.class));
			return onFailure.apply(err);
		}
		Either<T, Fail> conversion = slotValue.convertToJava(trace, clazz);
		if(conversion.isLeft()) {
			return conversion.left().value();
		}
		return onFailure.apply(conversion.right().value());
	}
	
    public default ClosedObject closed() {
        return new ClosedObject(this);
    }

}
