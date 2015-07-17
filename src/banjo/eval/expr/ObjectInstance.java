package banjo.eval.expr;

import banjo.event.Event;
import banjo.expr.util.ListUtil;
import banjo.value.FunctionTrait;
import banjo.value.Reaction;
import banjo.value.Value;
import banjo.value.ValueToStringTrait;
import fj.Ord;
import fj.P2;
import fj.data.List;
import fj.data.Option;
import fj.data.TreeMap;

public class ObjectInstance extends ValueToStringTrait implements Value {
	public final TreeMap<String, SlotInstance> slots;

	public ObjectInstance(
            TreeMap<String, SlotInstance> slots) {
		this.slots = slots;
    }

	@Override
	public Value slot(Value sourceObject, String name, Value fallback) {
		final Option<SlotInstance> value = slots.get(name);
		if(value.isSome())
			return maybeAnnotateAsMethod(value.some().apply(sourceObject, fallback), name);
		return Value.super.slot(sourceObject, name, fallback);
	}

	public class MethodInstance extends FunctionTrait implements Value {
		FunctionInstance f;
		public Value call(Value recurse, Value prevImpl, List<Value> arguments) {
			return f.call(recurse, prevImpl, arguments);
		}

		public Value call(List<Value> arguments) {
			return f.call(arguments);
		}

		public Value apply(List<Value> args) {
			return f.apply(args);
		}

		public final String name;
		
		public MethodInstance(FunctionInstance f, String name) {
			this.f = f;
			this.name = name;
		}

		@Override
		public String toStringFallback() {
			return ObjectInstance.this + "." + name;
		}
		
		
		@Override
		public Reaction<Value> react(Event event) {
			return f.react(event).map(this::update);
		}
		
		public MethodInstance update(Value newF) {
			if(newF == f)
				return this;
			return new MethodInstance(f, this.name);
		}
		
		@Override
		public boolean isDefined() {
			return f.isDefined();
		}
	}
	private Value maybeAnnotateAsMethod(Value value, String name) {
		if(value instanceof FunctionInstance) {
			return new MethodInstance((FunctionInstance)value, name);
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
	public Reaction<Value> react(Event event) {
		List<P2<String, SlotInstance>> pairs = List.iterableList(slots);
		List<SlotInstance> deps = pairs.map(P2.__2());
		Reaction<List<SlotInstance>> reactions = Reaction.to(deps, event);
		boolean changedSlots = deps != reactions.v;
		TreeMap<String, SlotInstance> newSlots = 
				changedSlots ? TreeMap.treeMap(Ord.stringOrd, pairs.map(P2.__1()).zip(reactions.v)) :
				this.slots;
		return reactions.from(newSlots).map(this::update);
	}

	private Value update(TreeMap<String, SlotInstance> newSlots) {
		return (slots == newSlots) ? this : new ObjectInstance(newSlots);
	}
}
