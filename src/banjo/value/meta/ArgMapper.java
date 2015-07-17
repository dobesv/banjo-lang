package banjo.value.meta;

import banjo.event.Event;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.P2;
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
	public Value call(Value recurse, Value baseImpl, List<Value> arguments) {
		return call(arguments);
	}
	
	@Override
	public Value call(List<Value> arguments) {
	    final List<Value> newArguments = arguments.map(List::single).map(f::call);
		return target.call(newArguments);
	}
	
	@Override
	public Reaction<Value> react(Event event) {
		Reaction<P2<Value, Value>> r = Reaction.to(f, target, event);
		Value newF = r.v._1();
		Value newTarget = r.v._2();
		Value newThis = (f == newF && target == newTarget) ? this : new ArgMapper(newF, newTarget);
		return r.from(newThis);
	}
	
	@Override
	public boolean isDefined() {
		return f.isDefined() && target.isDefined();
	}	
}
