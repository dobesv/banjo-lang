package banjo.eval.interceptors;

import banjo.eval.value.Value;
import fj.data.List;

/**
 * Transform all arguments using a given function
 */
public class ArgInterceptor extends Interceptor {
	public ArgInterceptor(Value interceptor, Value target) {
	    super(interceptor, target);
    }

	@Override
	public Value call(Value recurse, Value baseImpl, List<Value> arguments) {
	    final List<Value> newArguments = arguments.map(List::single).map(interceptor::call);
		return super.call(recurse, baseImpl, newArguments);
	}
}
