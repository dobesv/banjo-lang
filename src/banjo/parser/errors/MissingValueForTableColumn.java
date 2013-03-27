package banjo.parser.errors;

import banjo.parser.util.FileRange;

public class MissingValueForTableColumn extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public MissingValueForTableColumn(String message, FileRange range) {
		super(message, range);
	}

	public MissingValueForTableColumn(int column, int totalColumns, FileRange rowRange,
			String headingSource) {
		this("Missing value for column "+headingSource+" (column "+(column+1)+")", rowRange);
	}

}
