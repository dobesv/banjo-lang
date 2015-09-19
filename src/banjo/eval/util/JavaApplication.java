package banjo.eval.util;

import java.time.Clock;

import banjo.eval.Fail;
import banjo.eval.environment.Environment;
import banjo.event.Event;
import banjo.expr.core.CoreExpr;
import banjo.io.resource.Resource;
import banjo.value.EventChainValue;
import banjo.value.Reaction;
import banjo.value.SlotValue;
import banjo.value.TimeValue;
import banjo.value.Value;
import fj.P2;
import fj.data.List;

public class JavaApplication implements Runnable {
	public final TimeValue time;
	public final Resource resource;
	public final Reaction<Value> output;
	public final Clock clock;
	public final EventChainValue eventChain;
	
	public JavaApplication(TimeValue time, Resource resource, Clock clock, EventChainValue eventChain, Reaction<Value> output) {
		super();
		this.time = time;
		this.resource = resource;
		this.output = output;
		this.clock = clock;
		this.eventChain = eventChain;
	}
	public JavaApplication(Resource source, Clock clock, long fps, EventChainValue eventChain, Reaction<Value> output) {
		this(new TimeValue(clock.millis(), 1000L/fps),
				source, clock, eventChain, output);
	}

	public static Resource fatalErrorStartupEventEmitter(Fail f) {
		return Resource.startupEventEmitter(new Event(0, "fatal error", f));
	}
	public JavaApplication(Value app) {
		this.clock = app.readAndConvertSlot("clock", Clock.class, (f) -> Clock.systemUTC());
		long fps = app.readAndConvertSlot("fps", Long.class, (f) -> 60L);
		this.time = new TimeValue(clock.millis(), 1000L/fps);
		this.resource = app.readAndConvertSlot("resource", Resource.class, JavaApplication::fatalErrorStartupEventEmitter);
		this.eventChain = new EventChainValue(app.slot("pending event"), app.slot("event occurrence"));
		this.output = Reaction.of(app.call1(Value.fromJava(this)));
	}

	public static void defaultSleep(long millis) {
		try {
			Thread.sleep(millis);
		} catch (InterruptedException e) {
		}
	}

	@SlotName("epoch seconds")
	public Value epochSeconds() {
		// Return SlotValue so that it updates as time passes
		return new SlotValue(time, "epoch seconds");
	}

	@SlotName("timestamp")
	public Value epochMillis() {
		return new SlotValue(time, "epoch millis");
	}

	@SlotName("start timestamp")
	public Value startTimestamp() {
		// This value will not update on events
		return time.slot("epoch millis");
	}
	
	@SlotName("time")
	public Value getTime() {
		return time;
	}

	@SlotName("event chain")
	public EventChainValue getEventChain() {
		return eventChain;
	}
	
	public static JavaApplication fromExpr(final String rootExpr) {
		CoreExpr ast = CoreExpr.fromString(rootExpr);
		Environment environment = Environment.forCurrentDirectory();
		Value program = environment.eval(ast);
		return new JavaApplication(program);
	}
	
	public JavaApplication step() {
		long timestamp = clock.millis();
		P2<Resource, List<Event>> p = resource.poll(timestamp);
		Resource newResource = p._1();
		List<Event> newEvents = p._2();
		List<Event> newEventsSorted = newEvents.isEmpty() ? List.single(new Event(timestamp, "null")) : newEvents.sort(Event.ORD);
		long nextPollTime = resource.nextPollTime(timestamp);
		Reaction<Value> newOutput = new Reaction<Value>(output.v, nextPollTime);
		for(Event event : newEventsSorted) {
			// Update event and possibly reduce the expiry time
			newOutput = newOutput.v.react(event).maybeExpiring(newOutput.expiry);
			resource.accept(newOutput.v);
		}
		return update(newResource, newOutput);
	}
	private JavaApplication update(Resource newResource, Reaction<Value> newOutput) {
		if(newResource == this.resource && newOutput == this.output)
			return this;
		return new JavaApplication(time, newResource, clock, eventChain, newOutput);
	}

	@Override
	public void run() {
		run(this);
	}
	
	public static void run(JavaApplication app) {
		while (true) {
			app = app.step();
			long nextPollTime = app.output.expiry;
			if(nextPollTime == Long.MAX_VALUE) {
				// Probably done
				return;
			}
			long timestamp = app.clock.millis();
			if (nextPollTime > timestamp) {
				// TODO Custom sleep function for unit tests
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
