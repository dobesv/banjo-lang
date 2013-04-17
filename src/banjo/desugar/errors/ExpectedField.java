package banjo.desugar.errors;

import banjo.parser.errors.BanjoParseException;
import banjo.parser.util.FileRange;

public class ExpectedField extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public ExpectedField(FileRange range) {
		super("Expected key : value pair or '}'", range);
	}
}