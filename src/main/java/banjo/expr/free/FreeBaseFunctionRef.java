package banjo.expr.free;

import banjo.eval.NotCallable;
import banjo.eval.UnboundFunctionSelfName;
import banjo.eval.environment.BindingVisitor;
import banjo.eval.environment.Environment;
import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import fj.data.List;
import fj.data.Set;

public class FreeBaseFunctionRef implements FreeExpression {
	public final String name;
    public final Set<SourceFileRange> ranges;

    public FreeBaseFunctionRef(String name, Set<SourceFileRange> ranges) {
        super();
        this.name = name;
        this.ranges = ranges;
    }

	@Override
    public Value apply(Environment env, List<Value> trace) {
		return env.bindings.get(name).map(b -> b.acceptVisitor(new BindingVisitor<Value>() {
			
			@Override
			public Value functionRecursive(Value function) {
                return new NotCallable(trace, "No base implementation for function '" + name + "'", ranges);
			}
			
			@Override
			public Value functionRecursiveWithBase(Value function, Value baseFunction) {
				return baseFunction;
			}
			
			@Override
			public Value let(Value value) {
                return new UnboundFunctionSelfName(trace, "Not a function self-name: '" + name + "' is a regular let-bound variable", ranges);
			}
			
			@Override
			public Value slot(Value sourceObject, String slotName) {
                return new UnboundFunctionSelfName(trace, "Not a function self-name: '" + name + "' is a slot object reference", ranges);
			}
			
			@Override
			public Value slotWithBase(Value sourceObject, String slotName, Value baseSlotValue) {
                return new UnboundFunctionSelfName(trace, "Not a function self-name: '" + name + "' is a slot object reference", ranges);
			}
			
			
        })).orSome(() -> new UnboundFunctionSelfName(trace, "Not a function self-name: '" + name + "' is not a locally defined name", ranges));
	}
}