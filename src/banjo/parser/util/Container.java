package banjo.parser.util;

import org.eclipse.jdt.annotation.Nullable;

public class Container<T> {
	@Nullable private final T value;

	public Container(@Nullable T value) {
		this.value = value;
	}

	public @Nullable T getValue() {
		return this.value;
	}
}
