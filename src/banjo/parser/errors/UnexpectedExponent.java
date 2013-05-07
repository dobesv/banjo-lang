package banjo.parser.errors;

import banjo.parser.util.FileRange;

public class UnexpectedExponent extends Problem {
	private static final long serialVersionUID = 1L;

	public UnexpectedExponent(String message, FileRange range) {
		super(message, range);
	}

}