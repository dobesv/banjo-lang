package banjo.parser.errors;

import banjo.parser.util.FileRange;

public class IncorrectIndentation extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public IncorrectIndentation(FileRange range, int indentColumn) {
		this(range, indentColumn, false);
	}
	public IncorrectIndentation(FileRange range, int indentColumn, boolean orMore) {
		super("Expected indentation to column "+indentColumn+(orMore?" or more":"")+" but indentation was "+range.getStart().column+" columns.", range);
	}
}