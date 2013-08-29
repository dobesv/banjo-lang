package banjo.parser.util;

import org.eclipse.jdt.annotation.Nullable;

public class OffsetLength implements Comparable<OffsetLength> {
	final int offset;
	final int length;
	public int getOffset() {
		return this.offset;
	}
	public int getLength() {
		return this.length;
	}
	public OffsetLength(int offset, int length) {
		super();
		this.offset = offset;
		this.length = length;
	}
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + this.length;
		result = prime * result + this.offset;
		return result;
	}
	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof OffsetLength))
			return false;
		final OffsetLength other = (OffsetLength) obj;
		if (this.length != other.length)
			return false;
		if (this.offset != other.offset)
			return false;
		return true;
	}

	@Override
	public int compareTo(OffsetLength o) {
		final int cmp = Integer.compare(this.offset, o.offset);
		if(cmp != 0) return cmp;
		return Integer.compare(this.length, o.length);
	}
}