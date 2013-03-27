package banjo.parser;

import banjo.parser.errors.BanjoParseException;
import banjo.parser.util.FileRange;

public class ExtraTableColumn extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public ExtraTableColumn(String message, FileRange range) {
		super(message, range);
	}

	public ExtraTableColumn(int columnNumber, int headingCount, String valueProvided, FileRange range) {
		this("Unexpected extra column "+columnNumber+" ("+valueProvided+"); only "+headingCount+" headings defined for this table", range);
	}

}
