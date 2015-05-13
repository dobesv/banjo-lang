package interceptors;

import java.util.function.Function;

import banjo.eval.util.JavaRuntimeSupport;
import fj.data.List;

/**
 * Wrap a function with a call interceptor.
 *
 * The provided function will be called with a suspension tuple of the arguments given, which
 * the interceptor function can use to pass the same arguments to some other function(s).
 */
public class CallInterceptor extends Interceptor {

	public CallInterceptor(Object interceptor, Object target) {
		super(interceptor, target);
	}

	@Override
	public Object call(Object recurse, Object baseImpl, List<Object> arguments) {
		Function<Object, Object> suspension = new Function<Object, Object>() {
			@Override
			public Object apply(Object t) {
			    return JavaRuntimeSupport.call(t, arguments);
			}
		};
	    return JavaRuntimeSupport.call(interceptor, List.single(suspension));
	}

}
