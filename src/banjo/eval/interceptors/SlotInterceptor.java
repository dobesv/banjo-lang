package banjo.eval.interceptors;

import banjo.eval.util.JavaRuntimeSupport;
import banjo.eval.util.Selector;
import banjo.eval.value.Value;
import fj.data.List;

/**
 * Catch all slot accesses to this object and give them to a function
 * which can send the access to whatever other object(s) it wants.
 *
 * For example a slot access to "x.foo" could become a function
 * "tmp -> tmp.foo" which is passed to the function provided as
 * the interceptor.  That function can then read the given slot
 * on whatever objects it wants to.
 */
public class SlotInterceptor extends Interceptor {
	public SlotInterceptor(Value interceptor, Value target) {
	    super(interceptor, target);
    }

	@Override
	public Value slot(Value self, String name, Value fallback) {
		final Value sel = Value.fromJava(new Selector(name));
		return interceptor.call(List.list(sel, fallback));
	}
}
