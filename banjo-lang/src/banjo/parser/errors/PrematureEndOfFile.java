package banjo.parser.errors;

import banjo.parser.util.FileRange;

public class PrematureEndOfFile extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public PrematureEndOfFile(String message, FileRange range) {
		super(message, range);
	}

}