package banjo.event;

import java.util.IdentityHashMap;
import java.util.function.Function;

import com.sun.javafx.binding.ObjectConstant;

import banjo.expr.source.Operator;
import banjo.expr.util.ListUtil;
import banjo.expr.util.OrdUtil;
import banjo.value.FunctionTrait;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.Ord;
import fj.data.List;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;

public class PastEvent extends FunctionTrait implements Value, Function<Value,Value> {
	public static final Ord<PastEvent> ORD = OrdUtil.chain(
			Ord.longOrd.contramap(e -> e.timestamp),
			Ord.stringOrd.contramap(e -> e.variant),
			Ord.stringOrd.contramap(e -> e.toString())
	);
	
	public final long timestamp;
	public final String variant;
	public final Value arg;
	public final IdentityHashMap<Object, Reaction<?>> reactionCache = new IdentityHashMap<>();
	
	public PastEvent(long timestamp, String variant, Value arg) {
		super();
		this.timestamp = timestamp;
		this.variant = variant;
		this.arg = arg;
	}
	
	@Override
	public Value apply(Value t) {
		return t.callMethod(variant, List.single(this));
	}
	
	@Override
	public Reaction<Value> react(PastEvent event) {
		return Reaction.to(arg, event).map(this::update);
	}
	
	public PastEvent update(Value newArg) {
		if(newArg == this.arg)
			return this;
		return new PastEvent(timestamp, variant, newArg);
	}
	
	@Override
	public boolean isReactive() {
		return arg.isReactive();
	}

	@Override
	public String toString() {
		return "."+variant+"("+arg+")";
	}
	
	@Override
	public Value slot(Value self, String name, Value fallback) {
		switch(Operator.fromMethodName(name, true)) {
		case FALLBACK:
			return Value.function(x -> arg);
		case FUNCTION_COMPOSITION_RIGHT:
		case FUNCTION_COMPOSITION_LEFT: 
			return Value.function(f -> new PastEvent(timestamp, variant, f.call1(arg)));
		default:			
			if("timestamp".equals(name)) {
				return Value.fromJava(timestamp);
			}
			return super.slot(self, name, fallback);
		}
	}
	
	
	public static final class ObservablePastEvent extends ObjectBinding<Value> implements ObservableValue<Value>{
		final ObservableValue<Value> argBinding;
		PastEvent pastEvent;
		
		public ObservablePastEvent(PastEvent pastEvent) {
			this.pastEvent = pastEvent;
			argBinding = pastEvent.arg.toObservableValue();
		}
		
		@Override
		protected Value computeValue() {
			return pastEvent = pastEvent.update(argBinding.getValue());
		}
	}
	
	@Override
	public ObservableValue<Value> toObservableValue() {
		if(arg.isReactive()) {
			return new ObservablePastEvent(this);
		}
		return ObjectConstant.valueOf(this);
	}
}
