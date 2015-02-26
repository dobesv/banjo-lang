package banjo.parser.util;


public class OffsetLength {
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

	public int getEndOffset() {
		return this.offset + this.length;
	}

	@Override
	public String toString() {
		return "(offset " + this.offset + " length " + this.length + ")";
	}
}