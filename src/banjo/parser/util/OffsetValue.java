package banjo.parser.util;

import org.eclipse.jdt.annotation.Nullable;

public class OffsetValue<T> {

	protected final int offset;
	protected final T value;

	public OffsetValue(int offset, T value) {
		this.offset = offset;
		this.value = value;
	}

	public OffsetValue(OffsetValue<?> offset, T value) {
		this(offset.getOffset(), value);
	}

	public int getOffset() {
		return this.offset;
	}

	public T getValue() {
		return this.value;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + this.value.hashCode();
		result = prime * result + this.offset;
		return result;
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof OffsetExpr))
			return false;
		final OffsetExpr<?> other = (OffsetExpr<?>) obj;
		if (!this.value.equals(other.value))
			return false;
		if (this.offset != other.offset)
			return false;
		return true;
	}

}