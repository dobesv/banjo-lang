package banjo.event;

import java.util.function.Function;

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
		return t.callMethod(variant, t, null, args);
	}
	
	@Override
	public Reaction<Value> react(Event event) {
		return Reaction.to(args, event).map(this::updateArgs);
	}
	
	public Reaction<Event> reactE(Event event) {
		return Reaction.to(args, event).map(this::updateArgs);
	}
	
	public Event updateArgs(List<Value> args) {
		if(args == this.args)
			return this;
		return new Event(timestamp, variant, args);
	}
	
	public Event delay(long ms) {
		return at(timestamp + ms);
	}

	public Event at(long currentTimeMillis) {
		return new Event(currentTimeMillis, variant, args);
	}

	@Override
	public String toString() {
		return "."+variant+(args.isEmpty()?"":"("+ListUtil.insertCommas(args)+")");
	}
}
