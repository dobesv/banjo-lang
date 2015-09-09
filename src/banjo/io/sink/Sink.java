package banjo.io.sink;

import java.util.ServiceLoader;
import java.util.function.Consumer;

import banjo.value.Value;
import fj.Equal;
import fj.data.List;

public interface Sink extends Consumer<Value> {
	public static Sink discoveredEventSinksSink() {
		List<Sink> sinks = List.iterableList(ServiceLoader.load(Sink.class));
		return new CompositeSink(sinks);
	}
	
	public static List<Sink> flatten(Sink sink) {
		if(sink == NullSink.INSTANCE) {
			return List.nil();
		} else if(sink instanceof CompositeSink) {
			return ((CompositeSink)sink).sinks;
		} else {
			return List.single(sink);
		}
	}
	public static Sink merge(List<Sink> sinks) {
		sinks = List.join(sinks.map(Sink::flatten));
		if(sinks.isEmpty())
			return NullSink.INSTANCE;
		if(sinks.isSingle())
			return sinks.head();
		return new CompositeSink(sinks);
	}
	
	/**
	 * Convert a Consumer<Value> to a Sink.  This is trivial since a Sink
	 * is just a Consumer<Value> with a few extra methods.
	 */
	public static Sink fromConsumer(Consumer<Value> c) {
		return c::accept;
	}
	
	/**
	 * Return a sink which reads the given slot of the value it receives and
	 * sends it to this sink. 
	 */
	public default Sink slot(String name) {
		return (Value v) -> { accept(v.slot(name)); };
	}
	
	/**
	 * Create a Value which passes the given parameter to any value it receives
	 * as a function call, and passes the result to this sink.
	 */
	public default Sink call1(Value arg) {
		return (Value v) -> { accept(v.call1(arg)); };
	}

	/**
	 * Create a Value which passes the given parameters to any value it receives
	 * as a function call, and passes the result to this sink.
	 */
	public default Sink call(List<Value> args) {
		return (Value v) -> { accept(v.call(args)); };		
	}
	
	/**
	 * Create a sink which remembers its previous value (if any) and
	 * if a new value comes (checked using the eqCheck provided) then
	 * it passes it to this sink.  Otherwise it just throws it away.
	 */
	public default Sink dropRepeats(Equal<Value> eqCheck) {
		final Sink delegate = this;
		return new Sink() {
			Value lastValue = null;
			
			@Override
			public void accept(Value t) {
				if(lastValue == null || !(eqCheck.eq(lastValue, t)))
					delegate.accept(t);
				lastValue = t;
			}
		};
	}
	
	/**
	 * Create a sink which discards any "undefined" or "error" values
	 * and simply does not propagate them to this Sink.
	 */
	public default Sink dropUndefined() {
		return (Value v) -> { if(v.isDefined()) accept(v); };
	}
}
