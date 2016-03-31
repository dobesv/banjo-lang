package banjo.eval.util;

import java.time.Clock;

import banjo.eval.Fail;
import banjo.eval.environment.Environment;
import banjo.event.PastEvent;
import banjo.expr.core.CoreExpr;
import banjo.expr.util.SourceFileRange;
import banjo.io.resource.Resource;
import banjo.value.EventChainValue;
import banjo.value.Value;
import fj.P2;
import fj.data.List;

public class JavaApplication implements Runnable {
	public final Resource resource;
	public final Clock clock;
	public final EventChainValue eventChain;
	
	public JavaApplication(Resource resource, Clock clock, EventChainValue eventChain) {
		super();
		this.resource = resource;
		this.clock = clock;
		this.eventChain = eventChain;
	}

	public static Resource fatalErrorStartupEventEmitter(Fail f) {
		return Resource.startupEventEmitter(new PastEvent(0, "fatal error", f));
	}
	public JavaApplication(Value app) {
		this.clock = app.readAndConvertSlot("clock", Clock.class, (f) -> Clock.systemUTC());
		this.resource = app.readAndConvertSlot("resource", Resource.class, JavaApplication::fatalErrorStartupEventEmitter);
        this.eventChain = new EventChainValue(
            app.slot("pending event", SourceFileRange.EMPTY_SET),
            app.slot("event occurrence", SourceFileRange.EMPTY_SET));
		Value output = app.call1(resource);
		resource.watchValue(output);
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
		P2<Resource, List<PastEvent>> p = resource.poll(timestamp);
		Resource newResource = p._1();
		List<PastEvent> newEvents = p._2();
		List<PastEvent> newEventsSorted = newEvents.sort(PastEvent.ORD);
		newEventsSorted.forEach(newResource::handleEvent);
		return update(newResource);
	}
	private JavaApplication update(Resource newResource) {
		if(newResource == this.resource)
			return this;
		return new JavaApplication(newResource, clock, eventChain);
	}

	@Override
	public void run() {
		run(this);
	}
	
	public static void run(JavaApplication app) {
		while (true) {
			long lastPollTime = app.clock.millis();
			app = app.step();
			long nextPollTime = app.resource.nextPollTime(lastPollTime);
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
