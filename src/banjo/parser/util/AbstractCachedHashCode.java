package banjo.parser.util;

import org.eclipse.jdt.annotation.Nullable;

public class AbstractCachedHashCode {

	private final int hashCode;

	public AbstractCachedHashCode(int hashCode) {
		this.hashCode = hashCode;
	}

	@Override
	public final int hashCode() {
		return this.hashCode;
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if(obj == this) return true;
		return obj != null && obj.hashCode() == this.hashCode;
	}
}
