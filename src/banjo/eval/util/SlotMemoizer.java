package banjo.eval.util;

import java.util.HashMap;
import java.util.IdentityHashMap;

public class SlotMemoizer extends WrapperValue {
	public final HashMap<String, IdentityHashMap<Object, IdentityHashMap<Object, Object>>> cache = new HashMap<>();

	public SlotMemoizer(Object delegate) {
	    super(delegate);
    }

	@Override
	public Object slot(Object self, String name, Object fallback) {
		IdentityHashMap<Object, IdentityHashMap<Object, Object>> c1 = cache.get(name);
		if(c1 == null) cache.put(name, c1 = new IdentityHashMap<>());
		IdentityHashMap<Object, Object> c2 = c1.get(self);
		if(c2 == null) c1.put(self, c2 = new IdentityHashMap<>());
		Object value = c2.get(fallback);
		if(value == null) {
			c2.put(fallback, value = super.slot(self, name, fallback));
		}
		return value;
	}
}
