package banjo.expr.free;

import banjo.eval.SlotNotFound;
import banjo.eval.UnboundSlotSelfName;
import banjo.eval.environment.Binding;
import banjo.eval.environment.BindingVisitor;
import banjo.eval.environment.Environment;
import banjo.expr.source.Operator;
import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import fj.Ord;
import fj.P;
import fj.data.List;
import fj.data.Set;
import fj.data.TreeMap;

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
				// Create a special environment for the project, with all the slots for the object except
				// that the base slot value replaces the binding for slotName in that context.
				return projection.apply(new Environment(
                    object.apply(env, trace),
                    TreeMap.treeMap(Ord.stringOrd, List.single(P.p(slotName, Binding.let(baseSlotValue)))),
                    env.projectRootObject), trace);
			}
			
			@Override
			public Value functionRecursive(Value function) {
                return new UnboundSlotSelfName(trace, "Not a slot self-name: '" + id + "' is a function self-recursive name", ranges);
			}
			
			@Override
			public Value functionRecursiveWithBase(Value function, Value baseFunction) {
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