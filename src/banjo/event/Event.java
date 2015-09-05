package banjo.event;

import java.util.IdentityHashMap;
import java.util.function.Function;

import banjo.eval.util.JavaMethodValue;
import banjo.expr.util.ListUtil;
import banjo.expr.util.OrdUtil;
import banjo.value.FunctionTrait;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.Ord;
import fj.data.List;

public class Event extends FunctionTrait implements Value, Function<Value,Value> {
	public static final Ord<Event> ORD = OrdUtil.chain(
			Ord.longOrd.contramap(e -> e.timestamp),
			Ord.stringOrd.contramap(e -> e.variant),
			Ord.stringOrd.contramap(e -> e.toString())
	);
	
	public final long timestamp;
	public final String variant;
	public final List<Value> args;
	public final IdentityHashMap<Object, Reaction<?>> reactionCache = new IdentityHashMap<>();
	
	public Event(long timestamp, String variant, List<Value> args) {
		super();
		this.timestamp = timestamp;
		this.variant = variant;
		this.args = args;
	}
	
	public Event(long timestamp, String variant, Value arg) {
		this(timestamp, variant, List.single(arg));
	}
	
	public Event(long timestamp, String variant) {
		this(timestamp, variant, List.nil());
	}
	
	@Override
	public Value apply(Value t) {
		return t.callMethod(variant, t, null, args.cons(Value.fromJava(timestamp)));
	}
	
	@Override
	public Reaction<Value> react(Event event) {
		return Reaction.of(this);
	}
	
	public Reaction<Event> reactE(Event event) {
		return Reaction.of(this);
	}
	
	@Override
	public boolean isReactive() {
		return false;
	}
	
	public Event delay(long ms) {
		return at(timestamp + ms);
	}

	public Event at(long currentTimeMillis) {
		return new Event(currentTimeMillis, variant, args);
	}

	public Event map(String newVariant, Value f) {
		return new Event(timestamp, newVariant, f.call(args));
	}
	
	@Override
	public String toString() {
		return "."+variant+(args.isEmpty()?"":"("+ListUtil.insertCommas(args)+")");
	}
	
	@Override
	public Value slot(Value self, String name, Value fallback) {
		if("timestamp".equals(name)) {
			return Value.fromJava(timestamp);
		}
		return super.slot(self, name, fallback);
	}
}
