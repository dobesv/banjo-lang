package banjo.eval.util;

import java.util.function.Supplier;

/**
 * Provide an implementation of toString() that delegates to
 * the target value.
 */
public abstract class BaseSupplier implements Supplier<Object> {

	@Override
	public String toString() {
	    return String.valueOf(JavaRuntimeSupport.force(get()));
	}

}
