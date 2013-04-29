package banjo.parser.errors;

import banjo.parser.util.FileRange;

public class IncorrectIndentation extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public IncorrectIndentation(FileRange range, int expectedIndent, int actualIndet) {
		this(range, expectedIndent, actualIndet, false);
	}
	public IncorrectIndentation(FileRange range, int expectedIndent, int actualIndent, boolean orMore) {
		super("Expected indentation to column "+(orMore?">= ":"")+expectedIndent+" but indentation was "+actualIndent+" columns.", range);
	}
}