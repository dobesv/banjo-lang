package banjo.value.meta;

import java.util.HashMap;
import java.util.IdentityHashMap;

import banjo.value.Value;
import banjo.value.ValueVisitor;

public class SlotMemoizer extends WrapperValue {
	public final HashMap<String, IdentityHashMap<Value, IdentityHashMap<Value, Value>>> cache = new HashMap<>();

	public SlotMemoizer(Value delegate) {
	    super(delegate);
    }

//	@Override
//	public Value slot(Value self, String name, Value fallback) {
//		IdentityHashMap<Value, IdentityHashMap<Value, Value>> c1 = cache.get(name);
//		if(c1 == null) cache.put(name, c1 = new IdentityHashMap<>());
//		IdentityHashMap<Value, Value> c2 = c1.get(self);
//		if(c2 == null) c1.put(self, c2 = new IdentityHashMap<>());
//		Value value = c2.get(fallback);
//		if(value == null) {
//			c2.put(fallback, value = super.slot(self, name, fallback));
//		}
//		return value;
//	}
	
	@Override
	protected SlotMemoizer rewrap(Value newValue) {
		return new SlotMemoizer(newValue);
	}
	
    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return this.target.acceptVisitor(visitor);
    }
}
