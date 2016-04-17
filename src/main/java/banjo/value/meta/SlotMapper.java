package banjo.value.meta;

import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import banjo.value.ValueVisitor;
import fj.data.List;
import fj.data.Set;

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
	public Value slot(List<Value> trace, String name, Set<SourceFileRange> ranges) {
		return f.call(trace, List.single(source.slot(trace, name, ranges)));
	}
	
	@Override
	public Value slot(List<Value> trace, Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
		return slot(trace, name, ranges);
	}

	public SlotMapper update(Value newF, Value newSource) {
		if(f == newF && source == newSource)
			return this;
		return new SlotMapper(newF, newSource);
	}
	
	@Override
	public boolean isDefined(List<Value> trace) {
		return f.isDefined(trace) && source.isDefined(trace);
	}

    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.slotMapper(this);
    }
}
