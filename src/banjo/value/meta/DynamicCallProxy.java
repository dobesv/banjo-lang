package banjo.value.meta;

import java.util.function.Function;

import banjo.event.Event;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.data.List;

/**
 * Wrap a function with a call interceptor.
 *
 * The provided function will be called with a suspension tuple of the arguments given, which
 * the interceptor function can use to pass the same arguments to some other function(s).
 */
public class DynamicCallProxy implements Value {

	Value f;
	
	public DynamicCallProxy(Value f) {
		this.f = f;
	}

	@Override
	public Value call(Value recurse, Value baseImpl, List<Value> arguments) {
		Value suspension = Value.fromJava(new Function<Value, Value>() {
			@Override
			public Value apply(Value t) {
			    return t.call(arguments);
			}
		});
	    return f.call(List.single(suspension));
	}

	@Override
	public Reaction<Value> react(Event event) {
		return f.react(event).map(this::update);
	}

	public Value update(Value newF) {
		if(newF == f)
			return this;
		return new DynamicCallProxy(newF);
	}
	
	@Override
	public boolean isDefined() {
		return f.isDefined();
	}
}
