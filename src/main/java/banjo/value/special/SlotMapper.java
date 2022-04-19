package banjo.value.special;

import banjo.eval.EvalContext;
import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import banjo.value.ValueVisitor;
import fj.data.List;
import fj.data.Option;
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
	public Value slot(EvalContext<Value> ctx, String name, Set<SourceFileRange> ranges) {
		return f.call(ctx, List.single(source.slot(ctx, name, ranges)));
	}
	
	@Override
    public Value slot(EvalContext<Value> ctx, Value self, String name, Set<SourceFileRange> ranges, Option<Value> fallback) {
		return slot(ctx, name, ranges);
	}

	public SlotMapper update(Value newF, Value newSource) {
		if(f == newF && source == newSource)
			return this;
		return new SlotMapper(newF, newSource);
	}
	
	@Override
	public boolean isDefined(EvalContext<Value> ctx) {
		return f.isDefined(ctx) && source.isDefined(ctx);
	}

    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.slotMapper(this);
    }
}
