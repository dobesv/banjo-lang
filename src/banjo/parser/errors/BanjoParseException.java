package banjo.parser.errors;

import banjo.parser.util.FileRange;
import banjo.parser.util.OffsetLength;

public class BanjoParseException extends java.text.ParseException {
	private static final long serialVersionUID = 1L;
	private final int sourceLength;

	public BanjoParseException(String message, FileRange range) {
		this(message, range.getStartOffset(), range.length());
	}

	public BanjoParseException(String message, int sourceOffset, int sourceLength) {
		super(message, sourceOffset);
		this.sourceLength = sourceLength;
	}

	public BanjoParseException(String message, OffsetLength sourceOffsetLength) {
		super(message, sourceOffsetLength.getOffset());
		this.sourceLength = sourceOffsetLength.getLength();
	}

	public int getSourceOffset() {
		return getErrorOffset();
	}
	public int getSourceLength() {
		return this.sourceLength;
	}
}