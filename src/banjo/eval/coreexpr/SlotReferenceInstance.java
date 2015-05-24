package banjo.eval.coreexpr;

import banjo.eval.util.BaseSupplier;
import banjo.eval.util.JavaRuntimeSupport;

public class SlotReferenceInstance extends BaseSupplier {
	public final Object object;
	public final String name;

	public SlotReferenceInstance(Object object, String name) {
	    super();
	    this.object = object;
	    this.name = name;
    }

	@Override
	public Object get() {
		return JavaRuntimeSupport.readSlot(object, name);
	}
}
