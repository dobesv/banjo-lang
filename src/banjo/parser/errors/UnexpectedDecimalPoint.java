package banjo.parser.errors;

import banjo.parser.util.FileRange;

public class UnexpectedDecimalPoint extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public UnexpectedDecimalPoint(String message, FileRange range) {
		super(message, range);
	}

}