package banjo.eval.interceptors;

import java.util.function.Function;

import banjo.eval.value.Value;
import fj.data.List;

/**
 * Wrap a function with a call interceptor.
 *
 * The provided function will be called with a suspension tuple of the arguments given, which
 * the interceptor function can use to pass the same arguments to some other function(s).
 */
public class CallInterceptor extends Interceptor {

	public CallInterceptor(Value interceptor, Value target) {
		super(interceptor, target);
	}

	@Override
	public Value call(Value recurse, Value baseImpl, List<Value> arguments) {
		Value suspension = Value.fromJava(new Function<Value, Value>() {
			@Override
			public Value apply(Value t) {
			    return t.call(arguments);
			}
		});
	    return interceptor.call(List.single(suspension));
	}

}
