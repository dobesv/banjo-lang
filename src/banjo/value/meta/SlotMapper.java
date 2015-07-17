package banjo.value.meta;

import banjo.event.Event;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.P2;
import fj.data.List;

/**
 * Create an object by applying a function to each slot in the original
 * object to give the same slot names but a transformed value.
 * 
 * Note that this object never delegates slot references up the chain.
 */
public class SlotMapper implements Value {

	public final Value f;
	public final Value source;

	public SlotMapper(Value f, Value source) {
		this.f = f;
		this.source = source;
	}

	@Override
	public Value slot(String name) {
		return f.call(List.single(source.slot(name)));
	}
	
	@Override
	public Value slot(Value self, String name, Value fallback) {
		return slot(name);
	}

	@Override
	public Reaction<Value> react(Event event) {
		return Reaction.to(f,  source, event).map(P2.tuple(this::update));
	}

	public Value update(Value newF, Value newSource) {
		if(f == newF && source == newSource)
			return this;
		return new SlotMapper(newF, newSource);
	}
	
	@Override
	public boolean isDefined() {
		return f.isDefined() && source.isDefined();
	}
}
