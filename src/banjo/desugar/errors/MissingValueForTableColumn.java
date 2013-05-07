package banjo.desugar.errors;

import banjo.parser.errors.Problem;

public class MissingValueForTableColumn extends Problem {
	private static final long serialVersionUID = 1L;

	public MissingValueForTableColumn(int column, int totalColumns, int sourceOffset, int sourceLength,
			String headingSource) {
		super("Missing value for column "+headingSource+" (column "+(column+1)+")", sourceOffset, sourceLength);
	}

}
