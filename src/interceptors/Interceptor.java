package interceptors;

import banjo.eval.util.WrapperValue;


/**
 * Superclass for objects that wrap another object and intercept some
 * operations on that other object.
 */
public abstract class Interceptor extends WrapperValue {

	public final Object interceptor;
	public Interceptor(Object interceptor, Object target) {
	    super(target);
	    this.interceptor = interceptor;
    }
}