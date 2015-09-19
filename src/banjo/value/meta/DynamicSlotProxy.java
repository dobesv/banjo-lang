package banjo.value.meta;

import banjo.eval.util.Selector;
import banjo.event.Event;
import banjo.value.Reaction;
import banjo.value.Value;
import banjo.value.meta.FunctionComposition.ObservableFunctionComposition;
import fj.data.List;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;

/**
 * Catch all slot accesses to this object and give them to a function
 * which can send the access to whatever other object(s) it wants.
 *
 * For example a slot access to "x.foo" could become a function
 * "tmp -> tmp.foo" which is passed to the function provided as
 * the interceptor.  That function can then read the given slot
 * on whatever objects it wants to.
 */
public class DynamicSlotProxy implements Value {
	public final Value delegate;
	
	public DynamicSlotProxy(Value delegate) {
		this.delegate = delegate;
    }

	@Override
	public Value slot(Value self, String name, Value fallback) {
		final Value sel = new Selector(name);
		return delegate.call(List.list(sel, fallback));
	}

	@Override
	public Reaction<Value> react(Event event) {
		return delegate.react(event).map(this::update);
	}

	@Override
	public boolean isReactive() {
		return delegate.isReactive();
	}
	
	public DynamicSlotProxy update(Value newInterceptor) {
		if(newInterceptor == delegate)
			return this;
		return new DynamicSlotProxy(newInterceptor);
	}

	@Override
	public boolean isDefined() {
		return delegate.isDefined();
	}
	public static final class ObservableDynamicSlotProxy extends ObjectBinding<Value> {
		final ObservableValue<Value> delegateBinding;
		DynamicSlotProxy wrapperValue;
		
		public ObservableDynamicSlotProxy(DynamicSlotProxy wrapperValue) {
			super();
			delegateBinding = wrapperValue.delegate.toObservableValue();
			bind(delegateBinding);
			this.wrapperValue = wrapperValue;
		}
		
		@Override
		public void dispose() {
			unbind(delegateBinding);
		}
		
		@Override
		protected Value computeValue() {
			return wrapperValue = wrapperValue.update(delegateBinding.getValue());
		}
		
	}
	
	@Override
	public ObservableValue<Value> toObservableValue() {
		return new ObservableDynamicSlotProxy(this);
	}
	
}
