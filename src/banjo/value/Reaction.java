package banjo.value;

import banjo.event.Event;
import fj.F;
import fj.P;
import fj.P2;
import fj.P3;
import fj.data.List;

public class Reaction<T> {
	public final T v;
	public final long expiry;
	public Reaction(T newValue, long expiry) {
		super();
		this.v = newValue;
		this.expiry = expiry;
	}
	
	public static <A,B> Reaction<P2<A,B>> p(Reaction<A> a, Reaction<B> b) {
		return new Reaction<>(P.p(a.v, b.v), Math.min(a.expiry, b.expiry));
	}

	private static <A> A v(Reaction<A> a) {
		return a == null ? null : a.v;
	}
	private static long expiry(Reaction<?> a) {
		return a == null ? Long.MAX_VALUE : a.expiry;
	}
	public static <A,B,C> Reaction<P3<A,B,C>> p(Reaction<A> a, Reaction<B> b, Reaction<C> c) {
		return new Reaction<>(P.p(v(a), v(b), v(c)), Math.min(Math.min(expiry(a), expiry(b)), expiry(c)));
	}
	
	@SuppressWarnings("unchecked")
	public static <A> Reaction<A> to(Reactive<A> a, Event event) {
		if(a == null) return null;
		Reaction<A> result = (Reaction<A>) event.reactionCache.get(a);
		if(result != null)
			return result;
		result = a.react(event);
		event.reactionCache.put((Object)a, (Reaction<Object>)result);
		return result;
	}
	
	/**
	 * Call react on a pair of values and return a reaction with the resulting
	 * pair.
	 */
	public static <A,B> Reaction<P2<A,B>> to(Reactive<A> a, Reactive<B> b, Event event) {
		return p(to(a, event), to(b, event));
	}

	public static <A,B,C> Reaction<P3<A,B,C>> to(Reactive<A> a, Reactive<B> b, Reactive<C> c, Event event) {
		return p(to(a, event), to(b, event), to(c, event));
	}
	
	/**
	 * Check reactions on a list of dependent values.
	 * 
	 * If none of the values in the list change, the original list is returned.  This
	 * allows for an "==" check to be used as a kind of conservative but efficient
	 * no-change detection algorithm even on lists.
	 */
	public static <A extends Reactive<A>> Reaction<List<A>> to(List<A> deps, Event event) {
		List<Reaction<A>> reactions = deps.map(dep -> dep.react(event));
		long expiry = reactions.map(r -> r.expiry).foldRight(Math::min, Long.MAX_VALUE);
		List<A> newDeps = reactions.map(r -> r.v);
		boolean changed = newDeps.zipWith(deps, (a,b)-> (a != b)).foldRight((a,b) -> a || b, false);
		return new Reaction<List<A>>(changed?newDeps:deps, expiry);
	}
	
	/**
	 * Replace the value in the reaction, but leave the expiry as-is.
	 */
	public <A> Reaction<A> from(A a) {
		return new Reaction<A>(a, expiry);
	}

	/**
	 * Reaction of a non-reactive value.
	 */
	public static <T> Reaction<T> of(T t) {
		return new Reaction<T>(t, Long.MAX_VALUE);
	}

	public <B> Reaction<B> map(F<T,B> f) {
		return new Reaction<B>(f.f(v), expiry);
	}

	public Reaction<T> maybeExpiring(long minExpiry) {
		if(minExpiry < expiry)
			return new Reaction<T>(v, minExpiry);
		return this;
	}

}
