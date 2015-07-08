package banjo.expr.free;

import banjo.eval.expr.Environment;
import banjo.eval.value.Value;


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
		Value value = object.apply(env);
		return Value.lazy(() -> value.slot(slotName));
	}

	@Override
	public String toString() {
	    return object+"."+slotName;
	}
}