package banjo.desugar.errors;

import banjo.parser.errors.BanjoParseException;
import banjo.parser.util.OffsetLength;

public class MissingValueForTableColumn extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public MissingValueForTableColumn(String message, OffsetLength range) {
		super(message, range);
	}

	public MissingValueForTableColumn(int column, int totalColumns, OffsetLength offsetLength,
			String headingSource) {
		this("Missing value for column "+headingSource+" (column "+(column+1)+")", offsetLength);
	}

}
