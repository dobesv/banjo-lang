package banjo.value.special;

import banjo.eval.EvalContext;
import banjo.value.Value;
import banjo.value.ValueVisitor;
import fj.data.List;

/**
 * Transform all incoming arguments using a given function, and pass them to
 * the target function.
 */
public class ArgMapper implements Value {
	public final Value f;
	public final Value target;
	
	public ArgMapper(Value f, Value target) {
		super();
		this.f = f;
		this.target = target;
	}

	@Override
	public Value call(EvalContext<Value> ctx, Value recurse, Value baseImpl, List<Value> arguments) {
        return call(ctx, arguments);
	}
	
	@Override
	public Value call(EvalContext<Value> ctx, List<Value> arguments) {
        final List<Value> newArguments = arguments.map(arg -> f.call1(ctx, arg));
        return target.call(ctx, newArguments);
	}
	
	public ArgMapper update(Value newF, Value newTarget) {
		return (f == newF && target == newTarget) ? this : new ArgMapper(newF, newTarget);
	}
	
	@Override
	public boolean isDefined(EvalContext<Value> ctx) {
        return f.isDefined(ctx) && target.isDefined(ctx);
	}	
	
    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.argMapper(this);
    }
}
