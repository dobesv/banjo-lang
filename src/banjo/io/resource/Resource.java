package banjo.io.resource;

import java.util.ServiceLoader;
import java.util.function.Consumer;
import java.util.function.Predicate;

import banjo.event.Event;
import banjo.value.Value;
import fj.Equal;
import fj.P;
import fj.P2;
import fj.data.List;

public interface Resource extends Consumer<Value>, Value {
	public long nextPollTime(long lastPollTime);

	public P2<Resource, List<Event>> poll(long timestamp);
	
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
		return new BaseResource() {
			
			@Override
			public void accept(Value t) {
				delegate.accept(t.slot(name));
			}
			
			@Override
			public P2<Resource, List<Event>> poll(long timestamp) {
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
		};
	}
	
	/**
	 * Create a Value which passes the given parameter to any value it receives
	 * as a function call, and passes the result to this resource.
	 */
	public static Resource call1(final Resource delegate, final Value arg) {
		return new BaseResource() {
			
			@Override
			public void accept(Value t) {
				delegate.accept(t.call1(arg));
			}
			
			@Override
			public P2<Resource, List<Event>> poll(long timestamp) {
				return delegate.poll(timestamp).map1(r -> Resource.call1(r, arg));
			}
			
			@Override
			public long nextPollTime(long lastPollTime) {
				return delegate.nextPollTime(lastPollTime);
			}
		};
	}

	/**
	 * Create a Value which passes the given parameters to any value it receives
	 * as a function call, and passes the result to this resource.
	 */
	public static Resource call(final Resource delegate, final List<Value> args) {
		return new BaseResource() {
			
			@Override
			public void accept(Value t) {
				delegate.accept(t.call(args));
			}
			
			@Override
			public P2<Resource, List<Event>> poll(long timestamp) {
				return delegate.poll(timestamp).map1(r -> Resource.call(r, args));
			}
			
			@Override
			public long nextPollTime(long lastPollTime) {
				return delegate.nextPollTime(lastPollTime);
			}
		};
	}
	
	/**
	 * Create a resource which remembers its previous value (if any) and
	 * if a new value comes (checked using the eqCheck provided) then
	 * it passes it to this resource.  Otherwise it just throws it away.
	 */
	public default Resource ignoreRepeats(Equal<Value> eqCheck) {
		final Resource delegate = this;
		return new BaseResource() {
			Value lastValue = null;
			
			@Override
			public void accept(Value t) {
				if(lastValue == null || !(eqCheck.eq(lastValue, t)))
					delegate.accept(t);
				lastValue = t;
			}
			
			@Override
			public long nextPollTime(long lastPollTime) {
				return delegate.nextPollTime(lastPollTime);
			}
			
			@Override
			public P2<Resource, List<Event>> poll(long timestamp) {
				return delegate.poll(timestamp).map1(r -> r.ignoreRepeats(eqCheck));
			}
		};
	}
	
	/**
	 * Create a resource which discards any "undefined" or "error" values
	 * and simply does not propagate them to this Sink.
	 */
	public default Resource ignoreUndefined() {
		final Resource delegate = this;
		return new BaseResource() {
			@Override
			public void accept(Value t) {
				if(t != null && t.isDefined())
					delegate.accept(t);
			}
			
			@Override
			public long nextPollTime(long lastPollTime) {
				return delegate.nextPollTime(lastPollTime);
			}
			
			@Override
			public P2<Resource, List<Event>> poll(long timestamp) {
				return delegate.poll(timestamp).map1(r -> r.ignoreUndefined());
			}
		};
	}
	
	public default Resource ignoreIf(Predicate<Value> check) {
		final Resource delegate = this;
		return new BaseResource() {
			@Override
			public void accept(Value t) {
				if(check.test(t))
					delegate.accept(t);
			}
			
			@Override
			public long nextPollTime(long lastPollTime) {
				return delegate.nextPollTime(lastPollTime);
			}
			
			@Override
			public P2<Resource, List<Event>> poll(long timestamp) {
				return delegate.poll(timestamp).map1(r -> r.ignoreUndefined());
			}
		};
	}
	
	/**
	 * Create a resource which converts any value it receives to the given Java class
	 * and sends it to the given target.
	 * 
	 * If the value cannot be converted, the given fallback is passed instead.
	 * 
	 * This resource will not emit any events.
	 */
	public static <T> Resource pipeTo(Consumer<T> target, Class<T> clazz, T conversionFailureValue) {
		return new BaseResource() {
			
			@Override
			public void accept(Value v) {
				target.accept(v.convertToJava(clazz).left().orValue(conversionFailureValue));
			}
		};
	}
	
	/**
	 * Create a resource which converts any value it receives to the given Java class
	 * and sends it to the given target.
	 * 
	 * If the value cannot be converted, the value is thrown away.
	 */
	public static <T> Resource pipeTo(Consumer<T> target, Class<T> clazz) {
		return new BaseResource() {
			
			@Override
			public void accept(Value v) {
				v.convertToJava(clazz).left().forEach(target);
			}
		};
	}
	
	public static Resource startupEventEmitter(Event initialEvent) {
		return new BaseResource() {
			@Override
			public void accept(Value t) {
				// Ignore
			}
			
			@Override
			public long nextPollTime(long lastPollTime) {
				return lastPollTime;
			}
			
			@Override
			public P2<Resource, List<Event>> poll(long timestamp) {
				return P.p(NullResource.INSTANCE, List.single(initialEvent));
			}
		};
	}
}
