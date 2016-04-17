package banjo.value.meta;

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
	public Value call(List<Value> trace, Value recurse, Value baseImpl, List<Value> arguments) {
		return call(trace, arguments);
	}
	
	@Override
	public Value call(List<Value> trace, List<Value> arguments) {
        final List<Value> newArguments = arguments.map(arg -> f.call1(trace, arg));
		return target.call(trace, newArguments);
	}
	
	public ArgMapper update(Value newF, Value newTarget) {
		return (f == newF && target == newTarget) ? this : new ArgMapper(newF, newTarget);
	}
	
	@Override
	public boolean isDefined(List<Value> trace) {
		return f.isDefined(trace) && target.isDefined(trace);
	}	
	
    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.argMapper(this);
    }
}
