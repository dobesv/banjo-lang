package banjo.eval;

import banjo.eval.input.InputValue;
import banjo.eval.util.WrapperValue;
import fj.data.List;
import fj.data.Set;

public class Signal extends WrapperValue {
	public final Set<InputValue> dependencies;

	public Signal(Object target, Set<InputValue> dependencies) {
	    super(target);
	    this.dependencies = dependencies;
    }

	public static Signal constant(Object value) {
		return new Signal(value, Set.empty(InputValue.ord));
	}

	public static Signal input(Object value, InputValue input) {
		return new Signal(value, Set.single(InputValue.ord, input));

	}

	/** Create a signal if dependencies is not empty */
	public static Object addDependencies(Object x, Set<InputValue> dependencies) {
		if(dependencies.isEmpty())
			return x;
		else if(x instanceof Signal) {
			Signal s = (Signal)x;
			return new Signal(s.target, dependencies.union(s.dependencies));
		} else {
			return new Signal(x, dependencies);
		}
	}

	public Object wrap(Object x) {
		return addDependencies(x, dependencies);
	}

	@Override
	public Object call(Object recurse, Object baseImpl, List<Object> arguments) {
	    return wrap(super.call(recurse, baseImpl, arguments));
	}

	@Override
	public Object callMethod(String name, Object targetObject, Object fallback, List<Object> args) {
	    return wrap(super.callMethod(name, targetObject, fallback, args));
	}

	@Override
	public Object slot(Object self, String name, Object fallback) {
	    return wrap(super.slot(self, name, fallback));
	}
}
