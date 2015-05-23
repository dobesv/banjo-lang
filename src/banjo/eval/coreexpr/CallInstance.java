package banjo.eval.coreexpr;

import java.util.function.Supplier;

import banjo.eval.util.JavaRuntimeSupport;
import fj.data.List;

public class CallInstance implements Supplier<Object> {
	public final Object callee;
	public final List<Object> args;

	public CallInstance(Object callee, List<Object> args) {
		this.callee = callee;
		this.args = args;
    }

	@Override
	public Object get() {
		return JavaRuntimeSupport.call(callee, args);
	}

	@Override
	public String toString() {
	    return String.valueOf(JavaRuntimeSupport.force(get()));
	}

}
