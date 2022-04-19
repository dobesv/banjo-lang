package banjo.eval;

import banjo.expr.source.Operator;
import banjo.expr.util.SourceFileRange;
import banjo.value.MethodCallInstance;
import banjo.value.SlotValue;
import banjo.value.Value;
import banjo.value.ValueVisitor;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;

public class ExtendedObject implements Value {
    public static final class ChainedBaseFunctionImpl implements Value {
	    public final Value base;
	    public final Value baseCallable;

		public ChainedBaseFunctionImpl(Value base, Value baseCallable) {
	    	this.base = base;
	    	this.baseCallable = baseCallable;
        }

		@Override
	    public Value call(EvalContext<Value> ctx, Value callee, Value newBaseCallable, List<Value> arguments) {
	    	return base.call(ctx, callee, chainBaseImpl(baseCallable, newBaseCallable), arguments);
	    }
		
		public ChainedBaseFunctionImpl update(Value newBase, Value newPrevImpl) {
			return (newBase == base && newPrevImpl == baseCallable) ? this : new ChainedBaseFunctionImpl(newBase, newPrevImpl);
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
    public Value slot(EvalContext<Value> ctx, Value object, String name, Set<SourceFileRange> ranges,
            Option<Value> baseSlotValue) {
        final Value newBaseSlotValue = new SlotValue(base, object, name, ranges, baseSlotValue);
        return extension.slot(ctx, object, name, ranges, Option.some(newBaseSlotValue));
	}

	@Override
	public Value call(EvalContext<Value> ctx, Value recurse, Value prevImpl, List<Value> arguments) {
		return extension.call(ctx, recurse, chainBaseImpl(base, prevImpl), arguments);
	}

	private static Value chainBaseImpl(Value base, Value prevImpl) {
	    return prevImpl == null ? base : new ChainedBaseFunctionImpl(base, prevImpl);
    }

	@Override
	public Value callMethod(EvalContext<Value> ctx, String name, Set<SourceFileRange> ranges, Value targetObject, Value fallback, List<Value> args) {
        final Value newFallback = new MethodCallInstance(base, name, ranges, targetObject, fallback, args);
        return extension.callMethod(ctx, name, ranges, targetObject, newFallback, args);
	}

	@Override
    public String toStringFallback(EvalContext<Value> ctx) {
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

