package banjo.io.resource;

import java.util.ServiceLoader;
import java.util.function.Consumer;

import banjo.event.PastEvent;
import banjo.value.Reaction;
import banjo.value.SlotValue;
import banjo.value.Value;
import fj.P;
import fj.P2;
import fj.data.List;

public interface Resource extends Value {
	public static class StartEventEmitter extends BaseResource {
		final PastEvent initialEvent;
		
		
		public StartEventEmitter(PastEvent initialEvent) {
			super();
			this.initialEvent = initialEvent;
		}

		@Override
		public void handleEvent(PastEvent event) {
			// Ignore ...
		}

		@Override
		public void watchValue(Value output) {
			// Ignore ...
		}

		@Override
		public long nextPollTime(long lastPollTime) {
			return lastPollTime;
		}

		@Override
		public P2<Resource, List<PastEvent>> poll(long timestamp) {
			return P.p(NullResource.INSTANCE, List.single(initialEvent));
		}
	}
	public static class ChangeWatcher<T> extends BaseResource {
		private Reaction<Value> output;
		final Consumer<T> target;
		final Class<T> clazz;

		public ChangeWatcher(Consumer<T> target, Class<T> clazz) {
			this.target = target;
			this.clazz = clazz;
		}

		@Override
		public void watchValue(Value output) {
			this.output = Reaction.of(output);
			output.convertToJava(clazz).left().forEach(target);
		}

		@Override
		public void handleEvent(PastEvent event) {
			Value oldValue = output.v;
			output = Reaction.to(output.v, event);
			if(oldValue != output.v) {
				output.v.convertToJava(clazz).left().forEach(target);
			}
		}

		@Override
		public long nextPollTime(long lastPollTime) {
			return output.expiry;
		}
	}
	public static class ChangeWatcherWithConversionFailureValue<T> extends BaseResource {
		private Reaction<Value> output;
		final Consumer<T> target;
		final T conversionFailureValue;
		final Class<T> clazz;

		public ChangeWatcherWithConversionFailureValue(Consumer<T> target, Class<T> clazz, T conversionFailureValue) {
			this.target = target;
			this.clazz = clazz;
			this.conversionFailureValue = conversionFailureValue;
		}

		@Override
		public void watchValue(Value output) {
			this.output = Reaction.of(output);
			target.accept(output.convertToJava(clazz).left().orValue(conversionFailureValue));
		}

		@Override
		public void handleEvent(PastEvent event) {
			Value oldValue = output.v;
			output = Reaction.to(output.v, event);
			if(oldValue != output.v) {
				target.accept(output.v.convertToJava(clazz).left().orValue(conversionFailureValue));
			}
		}

		@Override
		public long nextPollTime(long lastPollTime) {
			return output.expiry;
		}
	}
	public static class FunctionCallingResource extends BaseResource {
		private final Resource delegate;
		private final List<Value> args;

		private FunctionCallingResource(Resource delegate, List<Value> args) {
			this.delegate = delegate;
			this.args = args;
		}

		@Override
		public void watchValue(Value t) {
			delegate.watchValue(t.call(args));
		}

		@Override
		public void handleEvent(PastEvent event) {
			delegate.handleEvent(event);
		}

		@Override
		public P2<Resource, List<PastEvent>> poll(long timestamp) {
			return delegate.poll(timestamp).map1(r -> Resource.call(r, args));
		}

		@Override
		public long nextPollTime(long lastPollTime) {
			return delegate.nextPollTime(lastPollTime);
		}
	}
	public static class SingleArgFunctionCallingResource extends BaseResource {
		private final Resource delegate;
		private final Value arg;

		private SingleArgFunctionCallingResource(Resource delegate, Value arg) {
			this.delegate = delegate;
			this.arg = arg;
		}

		@Override
		public void watchValue(Value t) {
			delegate.watchValue(t.call1(arg));
		}

		@Override
		public void handleEvent(PastEvent event) {
			delegate.handleEvent(event);
		}

		@Override
		public P2<Resource, List<PastEvent>> poll(long timestamp) {
			return delegate.poll(timestamp).map1(r -> Resource.call1(r, arg));
		}

		@Override
		public long nextPollTime(long lastPollTime) {
			return delegate.nextPollTime(lastPollTime);
		}
	}
	public static class SlotWatchingResource extends BaseResource {
		private final String name;
		private final Resource delegate;

		private SlotWatchingResource(String name, Resource delegate) {
			this.name = name;
			this.delegate = delegate;
		}

		@Override
		public void watchValue(Value t) {
			delegate.watchValue(new SlotValue(t, name));
		}

		@Override
		public void handleEvent(PastEvent event) {
			delegate.handleEvent(event);
		}

		@Override
		public P2<Resource, List<PastEvent>> poll(long timestamp) {
			return delegate.poll(timestamp).map1(r -> Resource.inSlot(name, r));
		}

		@Override
		public long nextPollTime(long lastPollTime) {
			return delegate.nextPollTime(lastPollTime);
		}

		@Override
		public Value slot(String slotName) {
			if(slotName.equals(name)) {
				return delegate;
			}
			return super.slot(slotName);
		}

		@Override
		public Value slot(Value self, String slotName, Value fallback) {
			if(slotName.equals(name)) {
				return delegate;
			}
			return super.slot(self, slotName, fallback);
		}
	}
	public long nextPollTime(long lastPollTime);

	public P2<Resource, List<PastEvent>> poll(long timestamp);
	
	public static Resource discovered() {
		List<Resource> resources = List.iterableList(ServiceLoader.load(Resource.class));
		return merge(resources);
	}
	
	public static List<Resource> flatten(Resource resource) {
		if(resource == NullResource.INSTANCE) {
			return List.nil();
		} else if(resource instanceof CompositeResource) {
			return ((CompositeResource)resource).resources;
		} else {
			return List.single(resource);
		}
	}
	public static Resource merge(List<Resource> resources) {
		resources = List.join(resources.map(Resource::flatten));
		if(resources.isEmpty())
			return NullResource.INSTANCE;
		if(resources.isSingle())
			return resources.head();
		return new CompositeResource(resources);
	}
	
	/**
	 * Return a resource which reads the given slot of the value it receives and
	 * sends it to this resource. 
	 */
	public static Resource inSlot(final String name, final Resource delegate) {
		return new SlotWatchingResource(name, delegate);
	}
	
	/**
	 * Create a Value which passes the given parameter to any value it receives
	 * as a function call, and passes the result to this resource.
	 */
	public static Resource call1(final Resource delegate, final Value arg) {
		return new SingleArgFunctionCallingResource(delegate, arg);
	}

	/**
	 * Create a Value which passes the given parameters to any value it receives
	 * as a function call, and passes the result to this resource.
	 */
	public static Resource call(final Resource delegate, final List<Value> args) {
		return new FunctionCallingResource(delegate, args);
	}
	
	/**
	 * Create a resource which converts any value it receives to the given Java class
	 * and sends it to the given target.
	 * 
	 * If the value cannot be converted, the given fallback is passed instead.
	 * 
	 * This resource will not emit any events.
	 */
	public static <T> Resource passChangesTo(Consumer<T> target, Class<T> clazz, T conversionFailureValue) {
		return new ChangeWatcherWithConversionFailureValue<T>(target, clazz, conversionFailureValue);
	}
	
	/**
	 * Create a resource which converts any value it receives to the given Java class
	 * and sends it to the given target.
	 * 
	 * If the value cannot be converted, the value is thrown away and the target is not invoked.
	 * 
	 * This resource does not emit any events.
	 */
	public static <T> Resource passChangesTo(Consumer<T> target, Class<T> clazz) {
		return new ChangeWatcher(target, clazz);
	}
	
	public static Resource startupEventEmitter(PastEvent initialEvent) {
		return new StartEventEmitter(initialEvent);
	}

	public void watchValue(Value output);
	public void handleEvent(PastEvent event);
}
