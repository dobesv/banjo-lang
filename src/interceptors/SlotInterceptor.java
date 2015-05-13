package interceptors;

import java.util.function.Supplier;

import fj.data.List;
import banjo.eval.SlotNotFound;
import banjo.eval.util.JavaRuntimeSupport;

/**
 * Apply some interceptor function to every slot in a given object, returning
 * a new object.  The interception of the slots is done lazily on slot access.
 */
public class SlotInterceptor extends Interceptor {
	public SlotInterceptor(Object interceptor, Object target) {
	    super(interceptor, target);
    }

	@Override
	public Object slot(Object self, String name, Supplier<Object> fallback) {
		Object targetSlotValue = JavaRuntimeSupport.readSlot(target, name);
		if(!JavaRuntimeSupport.isDefined(targetSlotValue)) {
			if(fallback != null)
				return fallback.get();
			return new SlotNotFound(name, target);
		}
		return JavaRuntimeSupport.call(interceptor, List.single(targetSlotValue));
	}
}
