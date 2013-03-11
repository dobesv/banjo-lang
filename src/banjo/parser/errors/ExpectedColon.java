package banjo.parser.errors;

import banjo.parser.util.FileRange;

public class ExpectedColon extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public ExpectedColon(String key, FileRange range) {
		super("Expected ':' after key '"+key+"'", range);
	}
}