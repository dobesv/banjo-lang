package banjo.eval;

import banjo.event.Event;
import banjo.expr.source.Operator;
import banjo.value.Reaction;
import banjo.value.Value;
import banjo.value.ValueToStringTrait;
import fj.P2;
import fj.data.List;

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

		public Value update(Value newBase, Value newPrevImpl) {
			return (newBase == base && newPrevImpl == prevImpl) ? this : new ChainedBaseFunctionImpl(newBase, newPrevImpl);
		}
    }

	public final Value base;
	public final Value extension;

	public ExtendedObject(Value base, Value extension) {
	    super();
	    this.base = base;
	    this.extension = extension;
    }

	@Override
	public Value slot(Value targetObject, String name, Value baseSlotValue) {
		final Value newBaseSlotValue =
			Value.lazy(() -> base.slot(targetObject, name, baseSlotValue));
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
		final Value newFallback = Value.lazy(() -> base.callMethod(name, targetObject, fallback, args));
		return extension.callMethod(name, targetObject, newFallback, args);
	}

	@Override
    public String toStringFallback() {
	    return base + " " + Operator.EXTENSION.getOp() + " " + extension;
	}
	
	@Override
	public Reaction<Value> react(Event event) {
		return Reaction.to(base, extension, event).map(P2.tuple(this::update));
	}

	public Value update(Value newBase, Value newExtenstion) {
		return (newBase == base && newExtenstion == extension) ? this : new ExtendedObject(newBase, newExtenstion);
	}



}

