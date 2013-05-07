package banjo.parser.errors;

import banjo.parser.util.FileRange;

public class ExpectedCloseBrace extends Problem {
	private static final long serialVersionUID = 1L;

	public ExpectedCloseBrace(String message, FileRange range) {
		super(message, range);
	}

	public ExpectedCloseBrace(FileRange range) {
		this("Expected '}'", range);
	}

}