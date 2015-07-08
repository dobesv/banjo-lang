package banjo.eval;

import banjo.eval.value.Value;
import banjo.eval.value.ValueToStringTrait;
import banjo.expr.source.Operator;
import fj.data.Either;
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
}
