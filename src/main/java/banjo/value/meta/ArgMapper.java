package banjo.value.meta;

import banjo.event.PastEvent;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.P2;
import fj.data.List;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;

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
	public Reaction<Value> react(PastEvent event) {
		Reaction<P2<Value, Value>> r = Reaction.to(f, target, event);
		Value newF = r.v._1();
		Value newTarget = r.v._2();
		Value newThis = update(newF, newTarget);
		return r.from(newThis);
	}

	public ArgMapper update(Value newF, Value newTarget) {
		return (f == newF && target == newTarget) ? this : new ArgMapper(newF, newTarget);
	}
	
	@Override
	public boolean isReactive() {
		return f.isReactive() || target.isReactive();
	}
	
	@Override
	public boolean isDefined() {
		return f.isDefined() && target.isDefined();
	}	
	
	public static final class ObservableArgMapper extends ObjectBinding<Value> {
		public final ObservableValue<Value> fBinding;
		public final ObservableValue<Value> targetBinding;
		public ArgMapper argMapper;
		public ObservableArgMapper(ArgMapper argMapper) {
			super();
			fBinding = argMapper.f.toObservableValue();
			targetBinding = argMapper.target.toObservableValue();
			bind(fBinding, targetBinding);
			this.argMapper = argMapper;
		}
		
		@Override
		public void dispose() {
			unbind(fBinding, targetBinding);
		}
		
		@Override
		protected Value computeValue() {
			return argMapper = argMapper.update(fBinding.getValue(), targetBinding.getValue());
		}
	}
	
	@Override
	public ObservableValue<Value> toObservableValue() {
		return new ObservableArgMapper(this);
	}
	
}
