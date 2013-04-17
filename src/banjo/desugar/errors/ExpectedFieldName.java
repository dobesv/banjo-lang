package banjo.desugar.errors;

import banjo.parser.errors.BanjoParseException;
import banjo.parser.util.FileRange;

public class ExpectedFieldName extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public ExpectedFieldName(String message, FileRange range) {
		super(message, range);
	}

}