package banjo.eval.util;

import java.time.Clock;
import java.util.function.Function;

import banjo.eval.Environment;
import banjo.eval.Environment;
import banjo.eval.Fail;
import banjo.event.Event;
import banjo.event.sink.EventSink;
import banjo.event.sink.NullEventSink;
import banjo.event.source.EventSource;
import banjo.expr.core.CoreExpr;
import banjo.value.Reaction;
import banjo.value.TimeValue;
import banjo.value.Value;
import fj.P2;
import fj.data.Either;
import fj.data.List;

public class JavaApplication implements Runnable {
	public final TimeValue time;
	public final EventSource source;
	public final EventSink sink;
	public final Reaction<Value> handler;
	public final Clock clock;

	
	public JavaApplication(TimeValue time, EventSource source, EventSink sink, Clock clock,
			Reaction<Value> handler) {
		super();
		this.time = time;
		this.source = source;
		this.sink = sink;
		this.handler = handler;
		this.clock = clock;
	}

	public static EventSource fatalErrorEventSource(Fail f) {
		return EventSource.of(new Event(0, "fatal error", f));
	}
	public JavaApplication(Value applicationSpec) {
		this.source = readAndConvertSlot(applicationSpec, "source", EventSource.class, JavaApplication::fatalErrorEventSource);
		this.sink = readAndConvertSlot(applicationSpec, "sink", EventSink.class, (f) -> NullEventSink.INSTANCE);
		this.clock = readAndConvertSlot(applicationSpec, "clock", Clock.class, (f) -> Clock.systemUTC());
		long fps = readAndConvertSlot(applicationSpec, "fps", Long.class, (f) -> 60L);
		this.time = new TimeValue(clock.millis(), 1000L/fps);
		this.handler = Reaction.none(applicationSpec.slot("handler"));
	}

	public static void defaultSleep(long millis) {
		try {
			Thread.sleep(millis);
		} catch (InterruptedException e) {
		}
	}

	@SlotName("epoch seconds")
	public Value epochSeconds() {
		return time.slot("epoch seconds");
	}

	@SlotName("epoch millis")
	public Value epochMillis() {
		return time.slot("epoch millis");
	}

	public void prompt(String message) {
		System.out.print(message);
	}

	@SlotName("no action")
	public void noAction() {
	}

	@SlotName("time")
	public Value time() {
		return time;
	}

	public static JavaApplication fromExpr(final String rootExpr) {
		CoreExpr ast = CoreExpr.fromString(rootExpr);
		Environment environment = Environment.forCurrentDirectory();
		Value program = environment.eval(ast);
		return new JavaApplication(program);
	}
	
	public static <T> T readAndConvertSlot(Value program, String slotName, Class<T> clazz, Function<Fail,T> onFailure) {
		Value slotValue = program.slot(slotName);
		if(!slotValue.isDefined()) {
			Fail err = Either.reduce(slotValue.convertToJava(Fail.class));
			return onFailure.apply(err);
		}
		Either<T, Fail> conversion = slotValue.convertToJava(clazz);
		if(conversion.isLeft()) {
			return conversion.left().value();
		}
		return onFailure.apply(conversion.right().value());
	}
	
	public JavaApplication step() {
		long timestamp = clock.millis();
		P2<EventSource, List<Event>> p = source.poll(timestamp);
		EventSource newSource = p._1();
		List<Event> newEvents = p._2();
		List<Event> newEventsSorted = newEvents.isEmpty() ? List.single(new Event(timestamp, "null")) : newEvents.sort(Event.ORD);
		Reaction<Value> newHandler = handler;
		while (!newEventsSorted.isEmpty()) {
			Event event = newEventsSorted.head();
			newEventsSorted = newEventsSorted.tail();
			sink.accept(event);

			newHandler = newHandler.v.react(event);
			
			Value updatedHandlerPlusAppEvents = newHandler.v.call(List.single(event));
			Value appEventsValue = updatedHandlerPlusAppEvents.slot("events");
			
			newHandler = Reaction.none(updatedHandlerPlusAppEvents.slot("handler"));
			
			@SuppressWarnings("rawtypes")
			Either<List, Fail> a = appEventsValue.convertToJava(List.class);
			readAndConvertSlot(appEventsValue, "events", List.class, (failure) -> { throw failure; });
			if(a.isLeft()) {
				@SuppressWarnings("unchecked")
				List<Object> b = (List<Object>) a.left().value();
				List<Event> appEvents = b.map((Object x) -> (Event)x);
				newEventsSorted = appEvents.foldLeft((q,e) -> q.insertBy(Event.ORD.compare(), e), newEventsSorted);
			} else {
				throw a.right().value();
			}
		}
		newHandler = newHandler.maybeExpiring(source.nextPollTime(timestamp));
		return update(newSource, newHandler);
	}
	private JavaApplication update(EventSource newSource, Reaction<Value> newHandler) {
		if(newSource == this.source && newHandler == this.handler)
			return this;
		return new JavaApplication(time, newSource, sink, clock, newHandler);
	}

	@Override
	public void run() {
		run(this);
	}
	
	public static void run(JavaApplication app) {
		while (true) {
			app = app.step();
			long nextPollTime = app.handler.expiry;
			if(nextPollTime == Long.MAX_VALUE) {
				// Probably done
				return;
			}
			long timestamp = app.clock.millis();
			if (nextPollTime > timestamp) {
				defaultSleep(nextPollTime - timestamp);
			}
		}
	}

	public static void main(String[] args) {
		for(String arg : args) {
			fromExpr(arg).run();
		}
	}
}
