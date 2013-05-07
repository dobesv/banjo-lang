package banjo.parser.errors;

import banjo.parser.util.FileRange;

public class ExpectedCloseParen extends Problem {
	private static final long serialVersionUID = 1L;

	public ExpectedCloseParen(String message, FileRange range) {
		super(message, range);
	}

	public ExpectedCloseParen(FileRange range) {
		this("Expected ')'", range);
	}

}