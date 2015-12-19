package banjo.value.meta;

import banjo.event.PastEvent;
import banjo.expr.util.SourceFileRange;
import banjo.value.BaseInertValue;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.data.Set;

public class SlotNames extends BaseInertValue implements Value {

	public static final Value INSTANCE = new SlotNames();

	@Override
	public Reaction<Value> react(PastEvent event) {
		return Reaction.of(this);
	}
	
	@Override
	public Value slot(Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
		return Value.fromJava(name);
	}
	
	@Override
	public Value slot(String name, Set<SourceFileRange> ranges) {
		return Value.fromJava(name);
	}
}
