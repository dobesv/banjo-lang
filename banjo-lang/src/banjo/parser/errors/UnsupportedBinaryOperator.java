package banjo.parser.errors;

import banjo.parser.util.FileRange;

public class UnsupportedBinaryOperator extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public UnsupportedBinaryOperator(String op, FileRange range) {
		super("Unsupported binary operator '"+op+"'", range);
	}
}