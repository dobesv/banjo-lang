package banjo.parser.errors;

import banjo.parser.util.FileRange;

public class ExpectedSemiColonOrNewline extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public ExpectedSemiColonOrNewline(FileRange range) {
		super("Expected semicolon or newline", range);
	}
}