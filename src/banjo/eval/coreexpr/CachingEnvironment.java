package banjo.eval.coreexpr;

import java.util.function.Function;

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
}