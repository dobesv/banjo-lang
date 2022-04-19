package banjo.value;

import banjo.eval.EvalContext;
import banjo.expr.util.SourceFileRange;
import fj.data.List;
import fj.data.Set;

public class MethodCallInstance extends CalculatedValue {
	public final Value object;
	public final String name;
    public final Set<SourceFileRange> ranges;
	public final Value targetObject;
	public final Value fallback;
	public final List<Value> args;
	
    public MethodCallInstance(Value object, String name, Set<SourceFileRange> ranges,
            Value targetObject, Value fallback, List<Value> args) {
		this.object = object;
		this.name = name;
        this.ranges = ranges;
		this.targetObject = targetObject;
		this.fallback = fallback;
		this.args = args;
	}
	
	@Override
	public Value calculate(EvalContext<Value> ctx) {
        return object.callMethod(ctx, name, ranges, targetObject, fallback, args);
	}

    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.methodCall(this);
    }
}