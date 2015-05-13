package interceptors;

import banjo.eval.Value;

/**
 * Superclass for objects that wrap another object and intercept some
 * operations on that other object.
 */
public abstract class Interceptor extends Value {

	public final Object interceptor;
	public final Object target;

	public Interceptor(Object interceptor, Object target) {
	    super();
	    this.interceptor = interceptor;
	    this.target = target;
    }

}