package banjo.eval.interceptors;

import banjo.eval.value.Value;
import banjo.eval.value.WrapperValue;


/**
 * Superclass for objects that wrap another object and intercept some
 * operations on that other object.
 */
public abstract class Interceptor extends WrapperValue {

	public final Value interceptor;
	public Interceptor(Value interceptor, Value target) {
	    super(target);
	    this.interceptor = interceptor;
    }
}