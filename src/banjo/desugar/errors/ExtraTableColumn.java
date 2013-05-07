package banjo.desugar.errors;

import banjo.parser.errors.Problem;

public class ExtraTableColumn extends Problem {
	private static final long serialVersionUID = 1L;

	public ExtraTableColumn(int columnNumber, int headingCount, String valueProvided, int sourceOffset, int sourceLength) {
		super("Unexpected extra column "+columnNumber+" ("+valueProvided+"); only "+headingCount+" headings defined for this table", sourceOffset, sourceLength);
	}

}
