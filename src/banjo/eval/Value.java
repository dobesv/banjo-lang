package banjo.eval;

import java.util.function.Supplier;

import banjo.eval.coreexpr.FunctionInstance;
import banjo.eval.util.JavaRuntimeSupport;
import fj.data.List;

public class Value {
	/**
	 * If this is a lazy value, evaluate it now and return a non-lazy value.
	 *
	 * Note that most of the methods below will automatically force the value
	 * if you call them.
	 */
	public Object calculate() { return this; }

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
	public Object call(Object recurse, Object baseImpl, List<Object> arguments) {
		if(baseImpl != null) {
			return JavaRuntimeSupport.call(baseImpl, recurse, null, arguments);
		}
		return new NotCallable(recurse);
	}

	/**
	 * Fetch the named slot.  If the object has no such slot, returns the baseSlotValue if provided, otherwise
	 * an error.
	 *
	 * @param self A reference to the object we are trying to get the slot from.  If the object is an extension
	 * then this may not be equal to "this".
	 * @param fallback Value to return if the slot is not found
	 */
	public Object slot(Object self, String name, Supplier<Object> fallback) {
		if(fallback != null)
			return fallback.get();
		return new SlotNotFound(name, self);
	}

	public int intValue() {
		Object v = JavaRuntimeSupport.force(slot(this, "int32 value", null));
		if(v instanceof Integer) {
			return ((Integer)v).intValue();
		}
		throw new IllegalStateException("Value is not an int32 and not convertible to int32: "+this, v instanceof Throwable ? (Throwable) v : null);
	}

	static final Supplier<Object> nullSupplier = new Supplier<Object>() {
		public Object get() { return null; }
	};

	/**
	 * Call a function in a slot.  A subclass may provide an optimized implementation in some cases.
	 *
	 * @param name Name of the method to call
	 * @param targetObject Original target object of the call if this callee is part of an extended object
	 * @param fallback If the method is not implemented, this lazily supplies a return value
	 * @param args List of arguments to pass
	 * @return
	 */
	public Object callMethod(String name, Object targetObject, Supplier<Object> fallback, List<Object> args) {
	    final Object f = slot(targetObject, name, nullSupplier);
	    if(fallback != null && !JavaRuntimeSupport.isDefined(f)) {
    		return fallback.get();
	    }
		return JavaRuntimeSupport.call(f, f, null, args);
    }

	@Override
	public String toString() {
	    try {
	    	return JavaRuntimeSupport.convertToJava(String.class, slot(this, "label", null));
	    } catch(Throwable t) {
	    	//t.printStackTrace();
	    	return toStringFallback();
	    }
	}

	protected String toStringFallback() {
	    return "<"+getClass().getSimpleName()+"(Value)>";
    }
}
