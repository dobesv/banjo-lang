package interceptors;

import banjo.eval.util.JavaRuntimeSupport;
import fj.data.List;

/**
 * Transform all arguments using a given function
 */
public class ArgInterceptor extends Interceptor {
	public ArgInterceptor(Object interceptor, Object target) {
	    super(interceptor, target);
    }

	@Override
	public Object call(Object recurse, Object baseImpl, List<Object> arguments) {
	    return super.call(recurse, baseImpl, arguments.map(arg -> JavaRuntimeSupport.call(interceptor, List.single(arg))));
	}
}
