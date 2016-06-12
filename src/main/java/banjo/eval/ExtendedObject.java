package banjo.eval;

import banjo.expr.source.Operator;
import banjo.expr.util.SourceFileRange;
import banjo.value.MethodCallInstance;
import banjo.value.SlotValue;
import banjo.value.Value;
import banjo.value.ValueToStringTrait;
import banjo.value.ValueVisitor;
import fj.data.List;
import fj.data.Set;

public class ExtendedObject extends ValueToStringTrait implements Value {
    public static final class ChainedBaseFunctionImpl extends ValueToStringTrait implements Value {
	    public final Value base;
	    public final Value prevImpl;

		public ChainedBaseFunctionImpl(Value base, Value prevImpl) {
	    	this.base = base;
	    	this.prevImpl = prevImpl;
        }

		@Override
	    public Value call(List<Value> trace, Value recurse, Value newPrevImpl, List<Value> arguments) {
	    	return base.call(trace, recurse, chainBaseImpl(prevImpl, newPrevImpl), arguments);
	    }
		
		public ChainedBaseFunctionImpl update(Value newBase, Value newPrevImpl) {
			return (newBase == base && newPrevImpl == prevImpl) ? this : new ChainedBaseFunctionImpl(newBase, newPrevImpl);
		}
		
        @Override
        public <T> T acceptVisitor(ValueVisitor<T> visitor) {
            return visitor.baseFunctionChain(this);
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
	public Value slot(List<Value> trace, Value object, String name, Set<SourceFileRange> ranges, Value baseSlotValue) {
        final Value newBaseSlotValue = new SlotValue(base, object, name, ranges, baseSlotValue);
		return extension.slot(trace, object, name, ranges, newBaseSlotValue);
	}

	@Override
	public Value call(List<Value> trace, Value recurse, Value prevImpl, List<Value> arguments) {
		return extension.call(trace, recurse, chainBaseImpl(base, prevImpl), arguments);
	}

	private static Value chainBaseImpl(Value base, Value prevImpl) {
	    return prevImpl == null ? base : new ChainedBaseFunctionImpl(base, prevImpl);
    }

	@Override
	public Value callMethod(List<Value> trace, String name, Set<SourceFileRange> ranges, Value targetObject, Value fallback, List<Value> args) {
        final Value newFallback = new MethodCallInstance(base, name, ranges, targetObject, fallback, args);
		return extension.callMethod(trace, name, ranges, targetObject, newFallback, args);
	}

	@Override
    public String toStringFallback(List<Value> trace) {
	    return base + " " + Operator.EXTENSION.getOp() + " " + extension;
	}
	
	public ExtendedObject update(Value newBase, Value newExtenstion) {
		return (newBase == base && newExtenstion == extension) ? this : new ExtendedObject(newBase, newExtenstion);
	}

    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.extendedObject(this);
    }
}

