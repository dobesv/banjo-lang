package banjo.parser.util;

public class OffsetLength {
	final int offset;
	final int length;
	public OffsetLength(int offset, int length) {
		super();
		this.offset = offset;
		this.length = length;
	}
	public int getOffset() {
		return this.offset;
	}
	public int getLength() {
		return this.length;
	}
	public int getEnd() {
		return this.offset + this.length;
	}

	@Override
	public String toString() {
		return "OffsetLength("+this.offset+","+this.length+")";
	}

}
