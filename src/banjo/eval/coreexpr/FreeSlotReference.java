package banjo.eval.coreexpr;

import banjo.eval.util.JavaRuntimeSupport;

public class FreeSlotReference implements FreeExpression {
	public final FreeExpression object;
	public final String slotName;


	public FreeSlotReference(FreeExpression object, String slotName) {
        super();
        this.object = object;
        this.slotName = slotName;
    }

	@Override
	public Object apply(Environment env) {
		return JavaRuntimeSupport.readSlot(object.apply(env), slotName);
	}

	@Override
	public String toString() {
	    return object+"."+slotName;
	}
}