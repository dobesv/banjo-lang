package banjo.expr.free;

import banjo.eval.Environment;
import banjo.value.SlotValue;
import banjo.value.Value;


public class FreeSlotReference implements FreeExpression {
	public final FreeExpression object;
	public final String slotName;


	public FreeSlotReference(FreeExpression object, String slotName) {
        super();
        this.object = object;
        this.slotName = slotName;
    }

	@Override
	public Value apply(Environment env) {
		return new SlotValue(object.apply(env), slotName);
	}

	@Override
	public String toString() {
	    return object+"."+slotName;
	}
}