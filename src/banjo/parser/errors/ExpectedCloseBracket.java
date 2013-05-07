package banjo.parser.errors;

import banjo.parser.util.FileRange;

public class ExpectedCloseBracket extends Problem {
	private static final long serialVersionUID = 1L;

	public ExpectedCloseBracket(String message, FileRange range) {
		super(message, range);
	}
	public ExpectedCloseBracket(FileRange range) {
		this("Expected ']'", range);
	}

}