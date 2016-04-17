package banjo.value.meta;

import banjo.value.Value;
import banjo.value.ValueVisitor;
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
	public Value call(List<Value> trace, Value recurse, Value baseImpl, List<Value> arguments) {
        Value suspension = Value.function(t -> t.call(trace, arguments));
	    return f.call(trace, List.single(suspension));
	}

	public DynamicCallProxy update(Value newF) {
		if(newF == f)
			return this;
		return new DynamicCallProxy(newF);
	}
	
	@Override
	public boolean isDefined(List<Value> trace) {
		return f.isDefined(trace);
	}
	
    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.dynamicCallProxy(this);
    }
}
