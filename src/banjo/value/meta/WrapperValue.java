package banjo.value.meta;

import banjo.eval.Fail;
import banjo.event.PastEvent;
import banjo.expr.util.SourceFileRange;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.data.Either;
import fj.data.List;
import fj.data.Set;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;

/**
 * Utility base class that wraps another object
 * and delegates all "Value" operations to that
 * object.
 *
 * A subclass can then override just the operations
 * that it needs to.
 */
public abstract class WrapperValue implements Value {

	public final Value target;
	private ObservableWrapperValue observable;

	public WrapperValue(Value target) {
		this.target = target;
    }

	@Override
    public Value call(Value recurse, Value baseImpl, List<Value> arguments) {
        return target.call(recurse, baseImpl, arguments);
    }

	@Override
    public Value callMethod(String name, Set<SourceFileRange> ranges, Value targetObject, Value fallback, List<Value> args) {
    	return target.callMethod(name, ranges, targetObject, fallback, args);
    }

	@Override
    public Value slot(Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
    	return target.slot(self, name, ranges, fallback);
    }

	@Override
	public Value force() {
	    return target.force();
    }

	@Override
	public boolean isDefined() {
	    return target.isDefined();
    }

	@Override
	public <T> Either<T, Fail> convertToJava(Class<T> clazz) {
	    return target.convertToJava(clazz);
    }

	@Override
	public String javaLabel() {
	    return target.javaLabel();
    }

	@Override
	public String toString() {
	    return target.toString();
	}

	@Override
	public Reaction<Value> react(PastEvent event) {
		return target.react(event).map(this::update);
	}

	@Override
	public boolean isReactive() {
		return target.isReactive();
	}
	
	public WrapperValue update(Value newValue) {
		if(newValue == target)
			return this;
		return this.rewrap(newValue);
	}
	
	protected abstract WrapperValue rewrap(Value newValue);
	
	public static final class ObservableWrapperValue extends ObjectBinding<Value> {
		final ObservableValue<Value> targetBinding;
		WrapperValue wrapperValue;
		
		public ObservableWrapperValue(WrapperValue wrapperValue) {
			super();
			targetBinding = wrapperValue.target.toObservableValue();
			bind(targetBinding);
			this.wrapperValue = wrapperValue;
		}
		
		@Override
		public void dispose() {
			unbind(targetBinding);
		}
		
		@Override
		protected Value computeValue() {
			return wrapperValue = wrapperValue.update(targetBinding.getValue());
		}
		
	}

	@Override
	public ObservableValue<Value> toObservableValue() {
		if(observable == null)
			observable = new ObservableWrapperValue(this);
		return observable;
	}
}