package banjo.eval.util;

import java.time.Clock;

import banjo.eval.Fail;
import banjo.eval.environment.Environment;
import banjo.event.Event;
import banjo.expr.core.CoreExpr;
import banjo.io.resource.Resource;
import banjo.value.EventChainValue;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.P2;
import fj.data.List;

public class JavaApplication implements Runnable {
	public final Resource resource;
	public final Reaction<Value> output;
	public final Clock clock;
	public final EventChainValue eventChain;
	
	public JavaApplication(Resource resource, Clock clock, EventChainValue eventChain, Reaction<Value> output) {
		super();
		this.resource = resource;
		this.output = output;
		this.clock = clock;
		this.eventChain = eventChain;
	}

	public static Resource fatalErrorStartupEventEmitter(Fail f) {
		return Resource.startupEventEmitter(new Event(0, "fatal error", f));
	}
	public JavaApplication(Value app) {
		this.clock = app.readAndConvertSlot("clock", Clock.class, (f) -> Clock.systemUTC());
		this.resource = app.readAndConvertSlot("resource", Resource.class, JavaApplication::fatalErrorStartupEventEmitter);
		this.eventChain = new EventChainValue(app.slot("pending event"), app.slot("event occurrence"));
		this.output = Reaction.of(app.call1(resource));
	}

	public static void defaultSleep(long millis) {
		try {
			Thread.sleep(millis);
		} catch (InterruptedException e) {
		}
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
		return new JavaApplication(newResource, clock, eventChain, newOutput);
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
