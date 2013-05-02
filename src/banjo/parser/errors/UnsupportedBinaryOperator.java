package banjo.parser.errors;

import banjo.parser.util.FileRange;
import banjo.parser.util.OffsetLength;

public class UnsupportedBinaryOperator extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public UnsupportedBinaryOperator(String op, OffsetLength offsetLength) {
		super("Unsupported binary operator '"+op+"'", offsetLength);
	}

	public UnsupportedBinaryOperator(String op, FileRange range) {
		super("Unsupported binary operator '"+op+"'", range);
	}
}