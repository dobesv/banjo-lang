package banjo.value.meta;

import banjo.event.PastEvent;
import banjo.expr.util.SourceFileRange;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.P2;
import fj.data.List;
import fj.data.Set;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;

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
	public Value slot(String name, Set<SourceFileRange> ranges) {
		return f.call(List.single(source.slot(name, ranges)));
	}
	
	@Override
	public Value slot(Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
		return slot(name, ranges);
	}

	@Override
	public Reaction<Value> react(PastEvent event) {
		return Reaction.to(f,  source, event).map(P2.tuple(this::update));
	}

	public SlotMapper update(Value newF, Value newSource) {
		if(f == newF && source == newSource)
			return this;
		return new SlotMapper(newF, newSource);
	}
	
	@Override
	public boolean isReactive() {
		return f.isReactive() || source.isReactive();
	}
	
	@Override
	public boolean isDefined() {
		return f.isDefined() && source.isDefined();
	}
	
	public static final class ObservableSlotMapper extends ObjectBinding<Value> {
		final ObservableValue<Value> fBinding;
		final ObservableValue<Value> sourceBinding;
		SlotMapper slotMapper;
		public ObservableSlotMapper(SlotMapper slotMapper) {
			super();
			fBinding = slotMapper.f.toObservableValue();
			sourceBinding = slotMapper.source.toObservableValue();
			bind(fBinding, sourceBinding);
			this.slotMapper = slotMapper;
		}
		
		@Override
		public void dispose() {
			unbind(fBinding, sourceBinding);
		}
		
		@Override
		protected Value computeValue() {
			return slotMapper = slotMapper.update(fBinding.getValue(), sourceBinding.getValue());
		}
		
		
	}

	@Override
	public ObservableValue<Value> toObservableValue() {
		return new ObservableSlotMapper(this);
	}
}
