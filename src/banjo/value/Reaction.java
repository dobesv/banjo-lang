package banjo.value;

import banjo.event.PastEvent;
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
		return new Reaction<>(P.p(a.v, b.v), minExpiry(a, b));
	}

	public static <A, B> long minExpiry(Reaction<A> a, Reaction<B> b) {
		return Math.min(expiry(a), expiry(b));
	}

	private static <A> A v(Reaction<A> a) {
		return a == null ? null : a.v;
	}
	private static long expiry(Reaction<?> a) {
		return a == null ? Long.MAX_VALUE : a.expiry;
	}
	public static <A,B,C> Reaction<P3<A,B,C>> p(Reaction<A> a, Reaction<B> b, Reaction<C> c) {
		return new Reaction<>(P.p(v(a), v(b), v(c)), minExpiry(a, b, c));
	}

	public static <A, B, C> long minExpiry(Reaction<A> a, Reaction<B> b, Reaction<C> c) {
		return Math.min(Math.min(expiry(a), expiry(b)), expiry(c));
	}
	
	@SuppressWarnings("unchecked")
	public static <A> Reaction<A> to(Reactive<A> a, PastEvent event) {
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
	public static <A,B> Reaction<P2<A,B>> to(Reactive<A> a, Reactive<B> b, PastEvent event) {
		return p(to(a, event), to(b, event));
	}

	public static <A,B,C> Reaction<P3<A,B,C>> to(Reactive<A> a, Reactive<B> b, Reactive<C> c, PastEvent event) {
		return p(to(a, event), to(b, event), to(c, event));
	}
	
	/**
	 * Check reactions on a list of dependent values.
	 * 
	 * If none of the values in the list change, the original list is returned.  This
	 * allows for an "==" check to be used as a kind of conservative but efficient
	 * no-change detection algorithm even on lists.
	 */
	@SuppressWarnings("unchecked")
	public static <A extends Reactive<A>> Reaction<List<A>> to(List<A> deps, PastEvent event) {
		Reaction<List<A>> result = (Reaction<List<A>>) event.reactionCache.get(deps);
		if(result != null)
			return result;
		List<Reaction<A>> reactions = deps.map(dep -> dep.react(event));
		long expiry = reactions.map(r -> r.expiry).foldRight(Math::min, Long.MAX_VALUE);
		List<A> newDeps = reactions.map(r -> r.v);
		boolean changed = newDeps.zipWith(deps, (a,b)-> (a != b)).foldRight((a,b) -> a || b, false);		
		result = new Reaction<List<A>>(changed?newDeps:deps, expiry);
		event.reactionCache.put(deps, result);
		return result;
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

	public Reaction<T> withExpiry(long expiry) {
		if(expiry == this.expiry)
			return this;
		return new Reaction<T>(this.v, expiry);
	}

}
