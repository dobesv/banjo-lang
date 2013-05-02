package banjo.desugar.errors;

import banjo.parser.errors.BanjoParseException;
import banjo.parser.util.OffsetLength;

public class ExtraTableColumn extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public ExtraTableColumn(String message, OffsetLength range) {
		super(message, range);
	}

	public ExtraTableColumn(int columnNumber, int headingCount, String valueProvided, OffsetLength offsetLength) {
		this("Unexpected extra column "+columnNumber+" ("+valueProvided+"); only "+headingCount+" headings defined for this table", offsetLength);
	}

}
