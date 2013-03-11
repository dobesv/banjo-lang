package banjo.parser.errors;

import banjo.parser.util.FileRange;

public class UnexpectedSecondDecimalPoint extends UnexpectedDecimalPoint {
	private static final long serialVersionUID = 1L;

	public UnexpectedSecondDecimalPoint(String message, FileRange range) {
		super(message, range);
	}

}