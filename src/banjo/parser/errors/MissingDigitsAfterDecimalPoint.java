package banjo.parser.errors;

import banjo.parser.util.FileRange;

public class MissingDigitsAfterDecimalPoint extends Problem {
	private static final long serialVersionUID = 1L;

	public MissingDigitsAfterDecimalPoint(String message, FileRange range) {
		super(message, range);
	}

}
