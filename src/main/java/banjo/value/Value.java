package banjo.value;

import java.util.function.Function;

import banjo.eval.ClosedObject;
import banjo.eval.EvalContext;
import banjo.eval.ExtendedObject;
import banjo.eval.expr.ObjectLiteralInstance;
import banjo.eval.expr.SlotInstance;
import banjo.expr.token.NumberLiteral;
import banjo.expr.util.SourceFileRange;
import banjo.value.fail.Fail;
import banjo.value.fail.FailWithMessage;
import banjo.value.fail.NotCallable;
import banjo.value.fail.SlotNotFound;
import fj.Ord;
import fj.data.Either;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.TreeMap;

public interface Value {

    /**
     * Allow values to be inspected and walked without using casts.
     */
    public <T> T acceptVisitor(ValueVisitor<T> visitor);

    /**
	 * If this is callable, perform the call and return the result. Otherwise
	 * return an error value.
	 * 
	 * @param ctx
	 *            TODO
	 * @param callee
	 *            A reference to the function being called, at the top level; if
	 *            the function calls itself recursively it should call that
	 *            function. This may be a different function from the one passed
	 *            if the function was an extension and it is calling up to its
	 *            base. If the base calls back to itself it should call the
	 *            extended version.
	 * @param baseCallable
	 *            If this object is an extension of another object, the other
	 *            object is provided as a base implementation that this function
	 *            can call. If this object isn't callable, then the base
	 *            implementation is used as a fallback. May be null.
	 */
    public default Value call(EvalContext<Value> ctx, Value callee, Value baseCallable, List<Value> arguments) {
		if(baseCallable != null) {
            return baseCallable.call(ctx, callee, null, arguments);
		}
        return new NotCallable(ctx, callee, SourceFileRange.EMPTY_SET);
	}

	/**
	 * Simple call with default recurse (the same object) and
	 * fallback (none).
	 * @param ctx TODO
	 */
    public default Value call(EvalContext<Value> ctx, List<Value> arguments) {
        return call(ctx, this, null, arguments);
	}
	
    public default Value call1(EvalContext<Value> ctx, Value v) {
        return call(ctx, List.single(v));
	}
	
	/**
	 * Fetch the named slot.  If the object has no such slot, returns the baseSlotValue if provided, otherwise
	 * an error.
	 * @param ctx TODO
	 * @param slotObjectRef A reference to the object we are trying to get the slot from.  If the object is an extension
	 * then this may not be equal to "this".
	 * @param ranges TODO
	 * @param fallback Value to return if the slot is not found
	 */
    public default Value slot(EvalContext<Value> ctx, Value slotObjectRef, String name, Set<SourceFileRange> ranges,
            Option<Value> fallback) {
        return fallback.orSome(() -> new SlotNotFound<Value>(ctx, name, ranges, slotObjectRef));
	}

	/**
     * Get the value of a slot.
     *
     * Returns an instance of SlotNotFound if the slot is not defined.
	 * @param ctx TODO
	 * @param ranges
     *            Source file ranges to blame if the slot is undefined
     */
    public default Value slot(EvalContext<Value> ctx, String name, Set<SourceFileRange> ranges) {
        return slot(ctx, this, name, ranges, null);
	}

    /**
     * Lookup a slot.
     * <p>
     * If you have relevant source file ranges available, use the one that takes
     * those ranges to improve debugging.
     * @param ctx TODO
     * @param name
     *            Name of the slot to get the value of
     * 
     * @return The value of the slot, or some <code>Fail<code> instance if the
     *         slot value is not defined for any reason
     */
    public default Value slot(EvalContext<Value> ctx, String name) {
        return slot(ctx, this, name, SourceFileRange.EMPTY_SET, null);
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
	 * @param ctx TODO
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
    public default Value callMethod(EvalContext<Value> ctx, String name, Set<SourceFileRange> ranges,
            Value targetObject, Value fallback, List<Value> args) {
        final Value f = slot(ctx, targetObject, name, ranges, null).force(ctx);
        if (fallback != null && !f.isDefined(ctx)) {
    		return fallback;
	    }
        return f.call(ctx, f, null, args);
    }

	/**
	 * Simple method call with defaults
	 * @param ctx TODO
	 */
    public default Value callMethod(EvalContext<Value> ctx, String name, Set<SourceFileRange> ranges,
            List<Value> args) {
        return callMethod(ctx, name, ranges, this, null, args);
	}

	/**
	 * If this value is lazy, calculate the "actual" value and return it.  This
	 * may be useful for checking the class of the object or certain kinds of
	 * optimization.  In general lazy values automatically "force" when you try
	 * to call them, access their slots, convert them to java, or call their methods.
	 * @param ctx TODO
	 */
    public default Value force(EvalContext<Value> ctx) {
		return this;
	}

	/**
	 * Return false if this is an "undefined" value - i.e. someone
	 * has tried to access an undefined slot or called the fail
	 * function or otherwise caused some kind of error.
	 * @param ctx TODO
	 */
    public default boolean isDefined(EvalContext<Value> ctx) {
		return true;
	}

	/**
	 * Return an instance of the given java class which is isomorphic to this value.
	 *
	 * The conversion should allow any loss of precision, generally this is only used
	 * to get a java equivalent of an object, not to encode/decode/convert/parse a
	 * value.
	 * @param ctx TODO
	 */
    public default <T> Either<T, Fail> convertToJava(EvalContext<Value> ctx, Class<T> clazz) {
        return Either.right(new FailWithMessage(ctx, "Cannot convert " + this + " to instance of " + clazz));
	}

	/**
     * Return true if this == true.
     */
    public default boolean isTrue(EvalContext<Value> ctx) {
        NumberLiteral one = new NumberLiteral(1, "1");
        NumberLiteral zero = new NumberLiteral(0, "0");
        TreeMap<String, SlotInstance<Value>> slots = TreeMap.<String, SlotInstance<Value>>empty(Ord.stringOrd)
                .set("true", SlotInstance.closed(one))
                .set("false", SlotInstance.closed(zero));
        ObjectLiteralInstance cb = new ObjectLiteralInstance(SourceFileRange.EMPTY_SET,
                slots);
        Value v = this.callMethod(ctx, "if", SourceFileRange.EMPTY_SET, List.single(cb));
        return v.acceptVisitor(new BaseValueVisitor<Boolean>() {
            @Override
            public Boolean fallback() {
                return false;
            }

            @Override
            public Boolean calculatedValue(CalculatedValue v) {
                return v.force(ctx).acceptVisitor(this);
            }

            @Override
            public Boolean number(NumberLiteral numberLiteral) {
                return numberLiteral.equals(one);
            }
        });
	}

	/**
     * Default implementation of toString(). Sadly we cannot call this method
     * "toString" here because we are an interface and interfaces cannot provide
     * a default toString().
	 * @param ctx TODO
     */
    public default String javaLabel(EvalContext<Value> ctx) {
	    try {
            Set<SourceFileRange> loc = SourceFileRange.currentJavaThreadLoc();
            Value label = slot(ctx, "label", loc);
            Value k = label.slot(ctx, "kernel string", loc);
            Either<String, Fail> j = k.convertToJava(ctx, String.class);
            return j.left().on(f -> toStringFallback(ctx));
	    } catch(Throwable t) {
            t.printStackTrace();
            return toStringFallback(ctx);
	    }
	}

	public default String toStringFallback(EvalContext<Value> ctx) {
	    return "<"+getClass().getSimpleName()+"(Value)>";
    }

    public default String stackTraceElementString() {
        return this.toString();
    }

	public default Value extendedWith(Value extension) {
		return new ExtendedObject(this, extension);
	}

	public default <T> T readAndConvertSlot(EvalContext<Value> ctx, String slotName, Class<T> clazz, Function<Fail,T> onFailure) {
        Value slotValue = this.slot(ctx, slotName, SourceFileRange.currentJavaThreadLoc());
        if (!slotValue.isDefined(ctx)) {
            Fail err = Either.reduce(slotValue.convertToJava(ctx, Fail.class));
			return onFailure.apply(err);
		}
        Either<T, Fail> conversion = slotValue.convertToJava(ctx, clazz);
		if(conversion.isLeft()) {
			return conversion.left().value();
		}
		return onFailure.apply(conversion.right().value());
	}
	
    public default ClosedObject closed() {
        return new ClosedObject(this);
    }

}
