package banjo.desugar.errors;

import banjo.parser.errors.BanjoParseException;
import banjo.parser.util.OffsetLength;

public class ExpectedField extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public ExpectedField(OffsetLength sourceOffsetLength) {
		super("Expected key : value pair or '}'", sourceOffsetLength);
	}
}