package banjo.expr.free;

import banjo.eval.Environment;
import banjo.eval.UnboundFunctionSelfName;
import banjo.eval.expr.BindingInstance;
import banjo.expr.source.Operator;
import banjo.value.ObjectBaseValue;
import banjo.value.Value;
import fj.Ord;
import fj.data.TreeMap;

public class FreeBaseProjection implements FreeExpression {
	public final FreeExpression object;
	public final FreeExpression projection;


	public FreeBaseProjection(FreeExpression object, FreeExpression projection) {
        super();
        this.object = object;
        this.projection = projection;
    }

	@Override
	public Value apply(Environment env) {
		String id = ((FreeIdentifier)object).id;
		final BindingInstance binding = env.get(id);
		if(binding.slotName != null) {
			Environment projectionEnvironment = new Environment(new ObjectBaseValue(id, binding.slotName, binding.baseSlotValue), env.rootEnvironment);
			return projection.apply(projectionEnvironment);
		} else {
			return new UnboundFunctionSelfName("Not a slot self-name: '"+id+"'");
		}
	}

	@Override
	public String toString() {
	    return object+Operator.BASE_SLOT.getOp()+projection;
	}
}