package banjo.eval.environment;

import banjo.eval.expr.BindingInstance;
import banjo.event.Event;
import banjo.value.Reaction;
import fj.Ord;
import fj.data.Option;
import fj.data.TreeMap;

public class CachingEnvironment implements Environment {
	TreeMap<String, BindingInstance> cache = TreeMap.empty(Ord.stringOrd);
	final Environment delegate;
	public CachingEnvironment(Environment delegate) {
        super();
        this.delegate = delegate;
    }
	@Override
	public BindingInstance apply(String t) {
		Option<BindingInstance> cached = cache.get(t);
		if(cached.isSome()) {
			return cached.some();
		}
		BindingInstance result = delegate.apply(t);
		cache = cache.set(t, result);
	    return result;
	}
	
	@Override
	public Reaction<Environment> react(Event event) {
		return delegate.react(event).map(this::update);
	}

	public Environment update(Environment newDelegate) {
		if(newDelegate == delegate)
			return this;
		return new CachingEnvironment(newDelegate);
	}
}