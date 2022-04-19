package banjo.eval.expr;

import banjo.eval.EvalContext;
import banjo.eval.resolver.ValueInstanceAlgebra;
import banjo.expr.util.ListUtil;
import banjo.expr.util.SourceFileRange;
import banjo.value.FunctionTrait;
import banjo.value.Value;
import banjo.value.ValueVisitor;
import fj.Ord;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.TreeMap;

public class ObjectLiteralInstance implements Value {
    public static final Value EMPTY = new ObjectLiteralInstance();
    public final Set<SourceFileRange> ranges;
    public final TreeMap<String, SlotInstance<Value>> slots;

    public ObjectLiteralInstance(Set<SourceFileRange> ranges, TreeMap<String, SlotInstance<Value>> slots) {
		super();
		this.ranges = ranges;
		this.slots = slots;
	}

    /**
     * Create an empty object value
     */
    private ObjectLiteralInstance() {
        this(SourceFileRange.EMPTY_SET, TreeMap.empty(Ord.stringOrd));
    }

    @Override
    public Value slot(EvalContext<Value> ctx, Value sourceObject, String name, Set<SourceFileRange> ranges,
            Option<Value> fallback) {
        return slots.get(name)
                .map(si -> si.apply(ctx, sourceObject, fallback, ValueInstanceAlgebra.INSTANCE))
            .map(sv -> maybeAnnotateAsMethod(sv, sourceObject, name))
            .orSome(() -> Value.super.slot(ctx, sourceObject, name, ranges, fallback));
	}

	public static class MethodInstance extends FunctionTrait implements Value {
		public final FunctionInstance f;
		public final Value sourceObject;
		public final String name;
		
		
		@Override
        public Value call(EvalContext<Value> ctx, Value recurse, Value prevImpl, List<Value> arguments) {
			return f.call(ctx, recurse, prevImpl, arguments);
		}

		@Override
        public Value call(EvalContext<Value> ctx, List<Value> arguments) {
			return f.call(ctx, arguments);
		}

        @Override
        public Value compose(Value functionAfter) {
            return f.compose(functionAfter);
        }

        @Override
        public Value call1(EvalContext<Value> ctx, Value v) {
            return f.call1(ctx, v);
        }

        @Override
        public Value slot(EvalContext<Value> ctx, Value self, String name, Set<SourceFileRange> ranges,
                Option<Value> fallback) {
            return f.slot(ctx, self, name, ranges, fallback);
        }

        @Override
        public Value slot(EvalContext<Value> ctx, String name, Set<SourceFileRange> ranges) {
            return f.slot(ctx, name, ranges);
        }

        @Override
        public Value slot(EvalContext<Value> ctx, String name) {
            return f.slot(ctx, name);
        }

        @Override
        public Value callMethod(EvalContext<Value> ctx, String name, Set<SourceFileRange> ranges, Value targetObject, Value fallback, List<Value> args) {
            return f.callMethod(ctx, name, ranges, targetObject, fallback, args);
        }

        @Override
        public Value callMethod(EvalContext<Value> ctx, String name, Set<SourceFileRange> ranges, List<Value> args) {
            return f.callMethod(ctx, name, ranges, args);
        }

		public MethodInstance(FunctionInstance f, Value sourceObject, String name) {
            super(f.trait);
			this.f = f;
			this.sourceObject = sourceObject;
			this.name = name;
		}

		@Override
		public String toStringFallback(EvalContext<Value> ctx) {
			return sourceObject + "." + name;
		}
		
		public MethodInstance update(Value newF) {
			if(newF == f)
				return this;
			return new MethodInstance(f, sourceObject, this.name);
		}
		
		@Override
		public boolean isDefined(EvalContext<Value> ctx) {
			return f.isDefined(ctx);
		}

        @Override
        public <T> T acceptVisitor(ValueVisitor<T> visitor) {
            return f.acceptVisitor(visitor);
        }
	}

    /**
     * If the value of a slot is a function, convert it into a MethodInstance
     * which has some extra information attached to it for easier debugging.
     */
	private Value maybeAnnotateAsMethod(Value value, Value sourceObject, String name) {
		if(value instanceof FunctionInstance) {
			return new MethodInstance((FunctionInstance)value, sourceObject, name);
		}
		return value;
	}

	@Override
	public String toStringFallback(EvalContext<Value> ctx) {
		StringBuffer sb = new StringBuffer();
		sb.append("{");
		ListUtil.insertCommas(sb, slots, slot -> {
			sb.append(slot._1()).append(" = ...");
		});
		sb.append("}");
	    return sb.toString();
	}
	
    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.objectLiteralInstance(this);
    }
}
