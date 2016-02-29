package banjo.eval.expr;

import banjo.event.PastEvent;
import banjo.expr.util.ListUtil;
import banjo.expr.util.SourceFileRange;
import banjo.value.FunctionTrait;
import banjo.value.Reaction;
import banjo.value.Value;
import banjo.value.ValueToStringTrait;
import fj.Ord;
import fj.P2;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.TreeMap;
import javafx.beans.Observable;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;

public class ObjectLiteralInstance extends ValueToStringTrait implements Value {
	public final Set<SourceFileRange> ranges;
	public final TreeMap<String, SlotInstance> slots;
	private ObservableObjectLiteralInstance observable;


	public ObjectLiteralInstance(Set<SourceFileRange> ranges, TreeMap<String, SlotInstance> slots) {
		super();
		this.ranges = ranges;
		this.slots = slots;
	}

	public ObjectLiteralInstance(TreeMap<String, Value> slotValues) {
		this(SourceFileRange.EMPTY_SET, slotValues.map(FreeSlotInstance::new));
	}

    /**
     * Create an empty object value
     */
    public ObjectLiteralInstance() {
        this(TreeMap.empty(Ord.stringOrd));
    }

    @Override
	public Value slot(Value sourceObject, String name, Set<SourceFileRange> ranges, Value fallback) {
		final Option<SlotInstance> value = slots.get(name);
		if(value.isSome())
			return maybeAnnotateAsMethod(value.some().apply(sourceObject, fallback), sourceObject, name);
		return Value.super.slot(sourceObject, name, ranges, fallback);
	}

	public static class MethodInstance extends FunctionTrait implements Value {
		public final FunctionInstance f;
		public final Value sourceObject;
		public final String name;
		
		
		@Override
        public Value call(Value recurse, Value prevImpl, List<Value> arguments) {
			return f.call(recurse, prevImpl, arguments);
		}

		@Override
        public Value call(List<Value> arguments) {
			return f.call(arguments);
		}

        @Override
        public Value compose(Value functionAfter) {
            return f.compose(functionAfter);
        }

        @Override
        public Value call1(Value v) {
            return f.call1(v);
        }

        @Override
        public Value slot(Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
            return f.slot(self, name, ranges, fallback);
        }

        @Override
        public Value slot(String name, Set<SourceFileRange> ranges) {
            return f.slot(name, ranges);
        }

        @Override
        public Value slot(String name) {
            return f.slot(name);
        }

        @Override
        public Value callMethod(String name, Set<SourceFileRange> ranges, Value targetObject, Value fallback, List<Value> args) {
            return f.callMethod(name, ranges, targetObject, fallback, args);
        }

        @Override
        public Value callMethod(String name, Set<SourceFileRange> ranges, List<Value> args) {
            return f.callMethod(name, ranges, args);
        }

        public Value apply(List<Value> args) {
			return f.apply(args);
		}

		public MethodInstance(FunctionInstance f, Value sourceObject, String name) {
			this.f = f;
			this.sourceObject = sourceObject;
			this.name = name;
		}

		@Override
		public String toStringFallback() {
			return sourceObject + "." + name;
		}
		
		
		@Override
		public Reaction<Value> react(PastEvent event) {
			return f.react(event).map(this::update);
		}
		
		@Override
		public boolean isReactive() {
			return f.isReactive();
		}
		
		public MethodInstance update(Value newF) {
			if(newF == f)
				return this;
			return new MethodInstance(f, sourceObject, this.name);
		}
		
		public static final class ObservableMethodInstance extends ObjectBinding<Value> {
			final ObservableValue<Value> fBinding;
			MethodInstance methodInstance;
			public ObservableMethodInstance(MethodInstance methodInstance) {
				super();
				fBinding = methodInstance.f.toObservableValue();
				bind(fBinding);
				this.methodInstance = methodInstance;
			}
			
			@Override
			public void dispose() {
				unbind(fBinding);
			}
			
			@Override
			protected Value computeValue() {
				return methodInstance = methodInstance.update(fBinding.getValue());
			}
		}
		
		@Override
		public ObservableValue<Value> toObservableValue() {
			return new ObservableMethodInstance(this);
		}
		
		@Override
		public boolean isDefined() {
			return f.isDefined();
		}
	}
	private Value maybeAnnotateAsMethod(Value value, Value sourceObject, String name) {
		if(value instanceof FunctionInstance) {
			return new MethodInstance((FunctionInstance)value, sourceObject, name);
		}
		return value;
	}

	@Override
	public String toStringFallback() {
		StringBuffer sb = new StringBuffer();
		sb.append("{");
		ListUtil.insertCommas(sb, slots, slot -> {
			sb.append(slot._1()).append(" = ...");
		});
		sb.append("}");
	    return sb.toString();
	}
	
	@Override
	public Reaction<Value> react(PastEvent event) {
		List<P2<String, SlotInstance>> pairs = List.iterableList(slots);
		List<SlotInstance> deps = pairs.map(P2.__2());
		Reaction<List<SlotInstance>> reactions = Reaction.to(deps, event);
		boolean changedSlots = deps != reactions.v;
		TreeMap<String, SlotInstance> newSlots = 
				changedSlots ? TreeMap.treeMap(Ord.stringOrd, pairs.map(P2.__1()).zip(reactions.v)) :
				this.slots;
		return reactions.from(newSlots).map(this::update);
	}

	@Override
	public boolean isReactive() {
		return slots.values().exists(si -> si.isReactive());
	}
	
	private ObjectLiteralInstance update(TreeMap<String, SlotInstance> newSlots) {
		return ListUtil.elementsEq(slots.values(), newSlots.values()) ? this : new ObjectLiteralInstance(ranges, newSlots);
	}
	
	public static final class ObservableObjectLiteralInstance extends ObjectBinding<Value> {
		final TreeMap<String, ObservableValue<SlotInstance>> slotBindings;
		ObjectLiteralInstance objectLiteralInstance;
		public ObservableObjectLiteralInstance(ObjectLiteralInstance objectLiteralInstance) {
			super();
			slotBindings = objectLiteralInstance.slots.map(SlotInstance::toObservableValue);
			bind(slotBindings.values().map(Observable.class::cast).array(Observable[].class));
			this.objectLiteralInstance = objectLiteralInstance;
		}
		
		@Override
		public void dispose() {
			unbind(slotBindings.values().map(Observable.class::cast).array(Observable[].class));
		}
		
		@Override
		protected Value computeValue() {
			return objectLiteralInstance = objectLiteralInstance.update(slotBindings.map(ObservableValue::getValue));
		}
	}
	
	@Override
	public ObservableValue<Value> toObservableValue() {
		if(observable == null)
			observable = new ObservableObjectLiteralInstance(this);
		return observable;
	}
}
