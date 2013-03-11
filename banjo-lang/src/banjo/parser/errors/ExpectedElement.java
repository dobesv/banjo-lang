package banjo.parser.errors;

import banjo.parser.util.FileRange;

public class ExpectedElement extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public ExpectedElement(FileRange range) {
		super("Expected comma, newline, or ']'", range);
	}
}