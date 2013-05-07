package banjo.desugar.errors;

import banjo.parser.errors.Problem;
import banjo.parser.util.OffsetLength;

public class ExpectedFieldName extends Problem {
	private static final long serialVersionUID = 1L;

	public ExpectedFieldName(String message, OffsetLength range) {
		super(message, range);
	}

}