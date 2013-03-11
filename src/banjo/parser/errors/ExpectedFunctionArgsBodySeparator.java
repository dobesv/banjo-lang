package banjo.parser.errors;

import banjo.parser.util.FileRange;

public class ExpectedFunctionArgsBodySeparator extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public ExpectedFunctionArgsBodySeparator(String message, FileRange range) {
		super(message, range);
	}

	public ExpectedFunctionArgsBodySeparator(int followedBy, FileRange fileRange) {
		this(new StringBuffer().append("Expected function argument list to be separated from body with '").appendCodePoint(followedBy).append("'").toString(), fileRange);
	}

}