package banjo.parser.errors;

import banjo.parser.util.FileRange;

public class MissingWhitespace extends Problem {
	private static final long serialVersionUID = 1L;

	public MissingWhitespace(String message, FileRange range) {
		super(message, range);
	}

}