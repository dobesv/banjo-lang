package banjo.eval.util;

import java.util.function.Supplier;

public class EagerSupplier extends BaseSupplier {

	private final Object value;

	public EagerSupplier(Object value) {
		this.value = value;
	}

	@Override
	public Object get() {
		return value;
	}

}
