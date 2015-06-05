package banjo.eval.util;

import fj.data.List;
import banjo.eval.Value;

/**
 * Utility base class that wraps another object
 * and delegates all "Value" operations to that
 * object.
 *
 * A subclass can then override just the operations
 * that it needs to.
 */
public class WrapperValue extends Value {

	public final Object target;

	public WrapperValue(Object target) {
		this.target = target;
    }

	@Override
    public Object call(Object recurse, Object baseImpl, List<Object> arguments) {
        return JavaRuntimeSupport.call(target, recurse, baseImpl, arguments);
    }

	@Override
    public Object callMethod(String name, Object targetObject, Object fallback, List<Object> args) {
    	return JavaRuntimeSupport.callMethod(target, name, targetObject, fallback, args);
    }

	@Override
    public Object slot(Object self, String name, Object fallback) {
    	return JavaRuntimeSupport.readSlot(target, self, fallback, name);
    }

}