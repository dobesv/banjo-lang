package banjo.expr.free;

import banjo.eval.SlotNotFound;
import banjo.eval.UnboundSlotSelfName;
import banjo.eval.environment.Binding;
import banjo.eval.environment.BindingVisitor;
import banjo.eval.environment.Environment;
import banjo.eval.expr.ObjectLiteralInstance;
import banjo.expr.source.Operator;
import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import fj.data.List;
import fj.data.Set;

/**
 * Projection that will use the "base" version of the current slot.
 * 
 * This is a bit tricky to understand and implement, unfortunately.  The
 * "base" slot value will be used for
 */
public class FreeBaseProjection implements FreeExpression {
	public final FreeIdentifier object;
	public final FreeExpression projection;


	public FreeBaseProjection(FreeIdentifier object, FreeExpression projection) {
        super();
        this.object = object;
        this.projection = projection;
    }

	@Override
    public Value apply(Environment env, List<Value> trace) {
		String id = object.id;
        Set<SourceFileRange> ranges = object.ranges;
		return env.bindings.get(id).map(b -> b.acceptVisitor(new BindingVisitor<Value>() {
			
			@Override
			public Value slot(Value sourceObject, String slotName) {
                return slotWithBase(sourceObject, slotName, new SlotNotFound(trace, id + ":" + slotName, ranges, sourceObject));
			}
			
			@Override
			public Value slotWithBase(Value sourceObject, String slotName, Value baseSlotValue) {
                // Create a special environment for the project, with only the
                // valid base slot defined
                Environment baseProjectionEnv = env.projection(ObjectLiteralInstance.EMPTY).bind(slotName, Binding.let(baseSlotValue));
                return projection.apply(baseProjectionEnv, trace);
			}
			
			@Override
            public Value functionSelf(Value function) {
                return new UnboundSlotSelfName(trace, "Not a slot self-name: '" + id + "' is a function self-recursive name", ranges);
			}
			
			@Override
            public Value functionSelfWithBase(Value function, Value baseFunction) {
                return new UnboundSlotSelfName(trace, "Not a slot self-name: '" + id + "' is a function self-recursive name", ranges);
			}
			
			@Override
			public Value let(Value value) {
                return new UnboundSlotSelfName(trace, "Not a slot self-name: '" + id + "' is a regular let binding", ranges);
			}
        })).orSome(() -> new UnboundSlotSelfName(trace, "Not a slot self-name: '" + id + "' is not defined", ranges));
	}

	@Override
	public String toString() {
	    return object+Operator.BASE_SLOT.getOp()+projection;
	}
}