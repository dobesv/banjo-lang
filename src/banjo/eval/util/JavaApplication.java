package banjo.eval.util;

import java.time.Clock;
import java.util.function.LongConsumer;
import java.util.function.Supplier;

import banjo.eval.Fail;
import banjo.eval.environment.ProjectEnvironment;
import banjo.event.Event;
import banjo.event.sink.ConsoleEventSink;
import banjo.event.sink.EventSink;
import banjo.event.sink.NullEventSink;
import banjo.event.source.EventSource;
import banjo.event.source.NilEventSource;
import banjo.event.source.StartupEventSource;
import banjo.event.view.EventView;
import banjo.event.view.EventFactoryEventView;
import banjo.expr.core.CoreExpr;
import banjo.expr.free.FreeExpression;
import banjo.expr.free.FreeExpressionFactory;
import banjo.value.Reaction;
import banjo.value.TimeValue;
import banjo.value.Value;
import fj.P2;
import fj.data.Either;
import fj.data.List;
import fj.data.Set;

public class JavaApplication implements Application {
	public final Value program;
	public final TimeValue time;
	public final LongConsumer sleep;

	public JavaApplication(Value program) {
		this(program, Clock.systemUTC(), JavaApplication::defaultSleep, EventSink::discoveredEventSinksSink,
				EventSource::discoveredEventSourcesSource);
	}

	public static void defaultSleep(long millis) {
		try {
			Thread.sleep(millis);
		} catch (InterruptedException e) {
		}
	}

	public JavaApplication(Value program, Clock clock, LongConsumer sleep, Supplier<EventSink> sinkFactory,
			Supplier<EventSource> sourceFactory) {
		this.program = program;
		this.time = new TimeValue(0);
		this.sleep = sleep;
	}

	@SlotName("epoch seconds")
	public Value epochSeconds() {
		return time.slot("epoch seconds");
	}

	@SlotName("epoch millis")
	public Value epochMillis() {
		return time.slot("epoch millis");
	}

	@SlotName("logger")
	public EventView logger(EventView values) {
		return values.as(ConsoleEventSink.LOG_EVENT_VARIANT);
	}

	@SlotName("on event")
	public EventView events(String variant) {
		return allEvents().filterEventVariant(variant);
	}
	
	@SlotName("on startup")
	public EventView startupEvent() {
		return events(StartupEventSource.STARTUP_EVENT_VARIANT);
	}
	
	public EventView log(Value message) {
		return logger(startupEvent().fmap(Value.function(v -> message)));
	}

	public void prompt(String message) {
		System.out.print(message);
	}

	@SlotName("no action")
	public void noAction() {
	}

	@SlotName("all events")
	public EventView allEvents() {
		return EventView.all;
	}

	@SlotName("time")
	public Value time() {
		return time;
	}

	public static Application fromExpr(final String rootExpr) {
		CoreExpr ast = CoreExpr.fromString(rootExpr);
		FreeExpression freeExpr = FreeExpressionFactory.apply(ast);
		String cwdPath = new java.io.File(".banjo").getAbsolutePath();
		ProjectEnvironment environment = ProjectEnvironment.forSourceFile(cwdPath);
		Value program = environment.bind(freeExpr);
		return new JavaApplication(program);
	}
	
	public <T> T readProgramSlot(String slotName, Class<T> clazz, Supplier<T> fallbackSupplier) {
		Value slotValue = program.slot(slotName);
		if(!slotValue.isDefined()) {
			slotValue.convertToJava(Fail.class).left().on(err -> new Fail("Unable to fetch program slot "+slotName+" from "+program)).printStackTrace();
			return fallbackSupplier.get();
		}
		Either<T, Fail> conversion = slotValue.convertToJava(clazz);
		if(conversion.isLeft()) {
			return conversion.left().value();
		}
		conversion.right().value().printStackTrace();
		return fallbackSupplier.get();
	}
	
	public void run() {
		final List<Value> progArgs = List.single(Value.fromJava(this));

		Clock clock = readProgramSlot("clock", Clock.class, Clock::systemUTC);
		EventSource source = readProgramSlot("source", EventSource.class, () -> EventSource.of(new Event(clock.millis(), "startup error")));
		EventSink sink = readProgramSlot("sink", EventSink.class, () -> NullEventSink.INSTANCE);
		Value outputs = program.call(progArgs);
		long outputsExpiry = 0;
		while (true) {
			long timestamp = clock.millis();
			P2<EventSource, List<Event>> p = source.poll(timestamp);
			source = p._1();
			List<Event> eventQueue = p._2().isEmpty() ? List.single(new Event(timestamp, "null")) : p._2().sort(Event.ORD);
			while (!eventQueue.isEmpty()) {
				Event event = eventQueue.head();
				eventQueue = eventQueue.tail();
				sink.accept(event);

				Reaction<Value> reaction = outputs.react(event);
				outputs = reaction.v;
				outputsExpiry = reaction.expiry;
				
				Either<EventView, Fail> a = outputs.convertToJava(EventView.class);
				if(a.isRight()) {
					new Fail("Application output is not an event view", a.right().value()).printStackTrace();
					return;
				}
				
				EventView appEventView = a.left().value();
				P2<EventView, List<Value>> b = appEventView.apply(event);
				List<Event> appEvents = b._2().map(x -> (Event)x);		
				eventQueue = appEvents.foldLeft((q,e) -> q.insertBy(Event.ORD.compare(), e), eventQueue);
				if(b._1() != appEventView) {
					outputs = Value.fromJava(b._1());
				}
			}
			long nextPollTime = Math.min(source.nextPollTime(timestamp), outputsExpiry);
			if(nextPollTime == Long.MAX_VALUE) {
				// Probably done
				return;
			}
			if (nextPollTime > timestamp) {
				sleep.accept(nextPollTime - clock.millis());
			}
		}
	}

	public void reportIfUndefined(Value value, String msg) {
		if(!value.isDefined()) {
			value.convertToJava(Fail.class).left().on(err -> new Fail(msg+value)).printStackTrace();
		}
	}

	public static void main(String[] args) {
		fromExpr(args[0]).run();
	}
}
