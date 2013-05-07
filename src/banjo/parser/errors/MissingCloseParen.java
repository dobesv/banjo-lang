package banjo.parser.errors;

import banjo.parser.util.FileRange;

public class MissingCloseParen extends Problem {
	private static final long serialVersionUID = 1L;

	public MissingCloseParen(FileRange range) {
		super("Missing ')'", range);
	}

}