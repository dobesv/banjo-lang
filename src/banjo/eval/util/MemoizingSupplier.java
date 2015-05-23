package banjo.eval.util;

import java.util.function.Supplier;

public class MemoizingSupplier<T> implements Supplier<T> {
	public final Supplier<T> delegate;
	public T value;
	public boolean calculated;
	public String label;

	public MemoizingSupplier(Supplier<T> delegate) {
	    super();
	    this.delegate = delegate;
    }


	@Override
	public T get() {
		if(!calculated) {
			calculated = true;
			value = delegate.get();
		}
		return value;
	}

	@Override
	public String toString() {
		if(label == null) {
			label = String.valueOf(delegate);
		}
	    return label;
	}
}
