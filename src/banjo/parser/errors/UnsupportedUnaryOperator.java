package banjo.parser.errors;

import banjo.parser.util.FileRange;
import banjo.parser.util.OffsetLength;

public class UnsupportedUnaryOperator extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public UnsupportedUnaryOperator(String op, OffsetLength offsetLength) {
		super("Unsupported unary operator '"+op+"'", offsetLength);
	}

	public UnsupportedUnaryOperator(String op, FileRange range) {
		super("Unsupported unary operator '"+op+"'", range);
	}
}