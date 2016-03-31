package banjo.expr.util;

import java.util.HashMap;

public class Interner<T> {

	private final HashMap<T,T> hash = new HashMap<T,T>();

	public T intern(T value) {
		final T existing = this.hash.get(value);
		if(existing != null)
			return existing;
		this.hash.put(value,value);
		return value;
	}

}
