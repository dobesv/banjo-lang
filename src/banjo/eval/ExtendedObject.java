package banjo.eval;

import com.sun.javafx.binding.ObjectConstant;

import banjo.event.Event;
import banjo.expr.source.Operator;
import banjo.value.MethodCallResultValue;
import banjo.value.Reaction;
import banjo.value.SlotValue;
import banjo.value.Value;
import banjo.value.ValueToStringTrait;
import fj.P2;
import fj.data.List;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;

public class ExtendedObject extends ValueToStringTrait implements Value {
	private static final class ChainedBaseFunctionImpl extends ValueToStringTrait implements Value {
	    public final Value base;
	    public final Value prevImpl;

		public ChainedBaseFunctionImpl(Value base, Value prevImpl) {
	    	this.base = base;
	    	this.prevImpl = prevImpl;
        }

		@Override
	    public Value call(Value recurse, Value newPrevImpl, List<Value> arguments) {
	    	return base.call(recurse, chainBaseImpl(prevImpl, newPrevImpl), arguments);
	    }
		
		@Override
		public Reaction<Value> react(Event event) {
			return Reaction.to(base, prevImpl, event).map(P2.tuple(this::update));
		}
		
		@Override
		public boolean isReactive() {
			return base.isReactive() || prevImpl.isReactive();
		}

		public ChainedBaseFunctionImpl update(Value newBase, Value newPrevImpl) {
			return (newBase == base && newPrevImpl == prevImpl) ? this : new ChainedBaseFunctionImpl(newBase, newPrevImpl);
		}
		
		public static final class ObservableChainedBaseFunctionImpl extends ObjectBinding<Value> {
			final ObservableValue<Value> baseBinding;
			final ObservableValue<Value> prevImplBinding;
			ChainedBaseFunctionImpl impl;
			
			public ObservableChainedBaseFunctionImpl(ChainedBaseFunctionImpl impl) {
				super();
				baseBinding = impl.base.toObservableValue();
				prevImplBinding = impl.prevImpl.toObservableValue();
				bind(baseBinding, prevImplBinding);
				this.impl = impl;
			}
			
			@Override
			public void dispose() {
				unbind(baseBinding, prevImplBinding);
			}
			
			@Override
			protected Value computeValue() {
				return impl = impl.update(baseBinding.getValue(), prevImplBinding.getValue());
			}
		}
		
		@Override
		public ObservableValue<Value> toObservableValue() {
			return new ObservableChainedBaseFunctionImpl(this);
		}
    }

	public final Value base;
	public final Value extension;
	public final boolean reactive;

	public ExtendedObject(Value base, Value extension) {
	    super();
	    this.base = base;
	    this.extension = extension;
	    this.reactive = base.isReactive() || extension.isReactive();
    }

	@Override
	public Value slot(Value targetObject, String name, Value baseSlotValue) {
		final Value newBaseSlotValue = new SlotValue(base, targetObject, name, baseSlotValue);
		return extension.slot(targetObject, name, newBaseSlotValue);
	}

	@Override
	public Value call(Value recurse, Value prevImpl, List<Value> arguments) {
		return extension.call(recurse, chainBaseImpl(base, prevImpl), arguments);
	}

	private static Value chainBaseImpl(Value base, Value prevImpl) {
	    return prevImpl == null ? base : new ChainedBaseFunctionImpl(base, prevImpl);
    }

	@Override
	public Value callMethod(String name, Value targetObject, Value fallback, List<Value> args) {
		final Value newFallback = new MethodCallResultValue(base, name, targetObject, fallback, args);
		return extension.callMethod(name, targetObject, newFallback, args);
	}

	@Override
    public String toStringFallback() {
	    return base + " " + Operator.EXTENSION.getOp() + " " + extension;
	}
	
	@Override
	public Reaction<Value> react(Event event) {
		if(!reactive)
			return Reaction.of(this);
		return Reaction.to(base, extension, event).map(P2.tuple(this::update));
	}
	
	@Override
	public boolean isReactive() {
		return reactive;
	}

	public ExtendedObject update(Value newBase, Value newExtenstion) {
		return (newBase == base && newExtenstion == extension) ? this : new ExtendedObject(newBase, newExtenstion);
	}


	public static class ObservableExtendedObject extends ObjectBinding<Value> {
		final ObservableValue<Value> baseBinding;
		final ObservableValue<Value> extensionBinding;
		ExtendedObject extendedObject;
		
		public ObservableExtendedObject(ExtendedObject extendedObject) {
			super();
			baseBinding = extendedObject.base.toObservableValue();
			extensionBinding = extendedObject.extension.toObservableValue();
			bind(baseBinding, extensionBinding);
			this.extendedObject = extendedObject;
		}
		
		@Override
		public void dispose() {
			unbind(baseBinding, extensionBinding);
		}
		
		@Override
		protected Value computeValue() {
			return extendedObject = extendedObject.update(baseBinding.getValue(), extensionBinding.getValue());
		}
	}
	
	@Override
	public ObservableValue<Value> toObservableValue() {
		if(reactive)
			return new ObservableExtendedObject(this);
		else
			return ObjectConstant.valueOf(this);
	}

}

