package banjo.expr.free;

import banjo.eval.UnboundFunctionSelfName;
import banjo.eval.expr.BindingInstance;
import banjo.eval.expr.Environment;
import banjo.expr.source.Operator;

public class FreeBaseSlotReference implements FreeExpression {
	public final FreeExpression object;
	public final String slotName;


	public FreeBaseSlotReference(FreeExpression object, String slotName) {
        super();
        this.object = object;
        this.slotName = slotName;
    }

	@Override
	public Object apply(Environment env) {
		String id = ((FreeIdentifier)object).id;
		final BindingInstance binding = env.apply(id);
		if(binding.bindsSelfForSlot(slotName))
			return binding.baseSlotValue;
		return new UnboundFunctionSelfName("Not a slot self-name: '"+id+"'");
	}

	@Override
	public String toString() {
	    return object+Operator.BASE_SLOT.getOp()+slotName;
	}
}