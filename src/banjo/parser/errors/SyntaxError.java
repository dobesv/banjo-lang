package banjo.parser.errors;

import banjo.parser.util.FileRange;

/**
 * Generic kind of "I have no idea what you've typed in here" error.
 */
public class SyntaxError extends Problem {
	private static final long serialVersionUID = 1L;

	public SyntaxError(FileRange range) {
		super("Syntax error", range);
	}

	public SyntaxError(String message, FileRange range) {
		super(message, range);
	}
}