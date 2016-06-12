package banjo.eval.expr;

import banjo.eval.resolver.ValueInstanceAlgebra;
import banjo.expr.util.ListUtil;
import banjo.expr.util.SourceFileRange;
import banjo.value.FunctionTrait;
import banjo.value.Value;
import banjo.value.ValueToStringTrait;
import banjo.value.ValueVisitor;
import fj.Ord;
import fj.data.List;
import fj.data.Set;
import fj.data.TreeMap;

public class ObjectLiteralInstance extends ValueToStringTrait implements Value {
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
	public Value slot(List<Value> trace, Value sourceObject, String name, Set<SourceFileRange> ranges, Value fallback) {
        return slots.get(name)
            .map(si -> si.apply(trace, sourceObject, fallback, ValueInstanceAlgebra.INSTANCE))
            .map(sv -> maybeAnnotateAsMethod(sv, sourceObject, name))
            .orSome(() -> Value.super.slot(trace, sourceObject, name, ranges, fallback));
	}

	public static class MethodInstance extends FunctionTrait implements Value {
		public final FunctionInstance f;
		public final Value sourceObject;
		public final String name;
		
		
		@Override
        public Value call(List<Value> trace, Value recurse, Value prevImpl, List<Value> arguments) {
			return f.call(trace, recurse, prevImpl, arguments);
		}

		@Override
        public Value call(List<Value> trace, List<Value> arguments) {
			return f.call(trace, arguments);
		}

        @Override
        public Value compose(Value functionAfter) {
            return f.compose(functionAfter);
        }

        @Override
        public Value call1(List<Value> trace, Value v) {
            return f.call1(trace, v);
        }

        @Override
        public Value slot(List<Value> trace, Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
            return f.slot(trace, self, name, ranges, fallback);
        }

        @Override
        public Value slot(List<Value> trace, String name, Set<SourceFileRange> ranges) {
            return f.slot(trace, name, ranges);
        }

        @Override
        public Value slot(List<Value> trace, String name) {
            return f.slot(trace, name);
        }

        @Override
        public Value callMethod(List<Value> trace, String name, Set<SourceFileRange> ranges, Value targetObject, Value fallback, List<Value> args) {
            return f.callMethod(trace, name, ranges, targetObject, fallback, args);
        }

        @Override
        public Value callMethod(List<Value> trace, String name, Set<SourceFileRange> ranges, List<Value> args) {
            return f.callMethod(trace, name, ranges, args);
        }

		public MethodInstance(FunctionInstance f, Value sourceObject, String name) {
			this.f = f;
			this.sourceObject = sourceObject;
			this.name = name;
		}

		@Override
		public String toStringFallback(List<Value> trace) {
			return sourceObject + "." + name;
		}
		
		public MethodInstance update(Value newF) {
			if(newF == f)
				return this;
			return new MethodInstance(f, sourceObject, this.name);
		}
		
		@Override
		public boolean isDefined(List<Value> trace) {
			return f.isDefined(trace);
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
	public String toStringFallback(List<Value> trace) {
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
