package banjo.eval.coreexpr;

import banjo.eval.util.BaseSupplier;
import banjo.eval.util.JavaRuntimeSupport;
import fj.data.List;

public class CallInstance extends BaseSupplier {
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

}
