package banjo.eval.interceptors;

import java.util.function.Function;
import java.util.function.Supplier;

import fj.data.List;
import banjo.eval.SlotNotFound;
import banjo.eval.util.JavaRuntimeSupport;

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
	public SlotInterceptor(Object interceptor, Object target) {
	    super(interceptor, target);
    }

	static class Selector implements Function<Object, Object> {
		public final String slotName;
		public Selector(String slotName) {
			this.slotName = slotName;
		}

		@Override
		public String toString() {
		    return "."+slotName;
		}

		@Override
        public Object apply(Object t) {
	        return JavaRuntimeSupport.readSlot(t, slotName);
        }
	}
	@Override
	public Object slot(Object self, String name, Object fallback) {
		final Selector sel = new Selector(name);
		return JavaRuntimeSupport.call(interceptor, List.list(sel, fallback));
	}
}
