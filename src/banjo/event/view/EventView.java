package banjo.event.view;

import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;

import banjo.eval.util.SlotName;
import banjo.event.Event;
import banjo.event.source.EventSource;
import banjo.value.Reactive;
import banjo.value.Value;
import fj.P2;
import fj.data.List;

public interface EventView extends Function<Event, P2<EventView, List<Value>>>, Reactive<EventView> {

	public static final EventView all = new AllEventView();

	@SlotName("∘")
	public default EventView fmap(Value function) {
		return new TransformingEventView(this, function);
	}

	public default EventView filter(Value predicate) {
		return new FilteringEventView(this, predicate);
	}
	
	@SlotName("aggregate")
	public default EventView aggregating(BiFunction<Value,Value,Value> f, Value init) {
		return new AggregatingEventView(this, f, init);
	}

	@SlotName("echoing")
	public default EventView echoing(long period, int count) {
		return new EchoingEventView(this, period, count);
	}

	@SlotName("?:")
	public default Value toValueWithDefault(Value defaultValue) {
		return new ValueEventView(this, defaultValue);
	}
	
	@SlotName("variant")
	public default EventView filterEventVariant(String variant) {
		return filter(Value.function(v -> Value.fromJava((v instanceof Event) && ((Event)v).variant.equals(variant))));
	}

	/**
	 * Take the values we currently have after filtering and mapping
	 * and construct an event from it.
	 */
	@SlotName("as")
	public default EventView as(String variant) {
		return new EventFactoryEventView(this, variant);
	}
	
	@SlotName("∪")
	public default EventView union(EventView other) {
		return EventView.merge(List.list(this, other));
	}

	@SlotName("count")
	public default Value count() {
		return counts().toValueWithDefault(Value.fromJava(0L));
	}

	@SlotName("counter")
	public default EventView counts() {
		return new CountingEventView(this, 0L);
	}
	
	public static EventView merge(List<EventView> views) {
		return new CompositeEventView(views);
	}
}
