package banjo.desugar.errors;

import banjo.parser.errors.BanjoParseException;
import banjo.parser.util.OffsetLength;

public class InvalidProjection extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public InvalidProjection(String src, OffsetLength range) {
		super("Invalid projection "+src, range);
	}
}
