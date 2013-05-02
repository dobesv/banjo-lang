package banjo.desugar.errors;

import banjo.dom.token.Ellipsis;
import banjo.parser.errors.BanjoParseException;
import banjo.parser.util.OffsetLength;

public class UnexpectedEllipsis extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public UnexpectedEllipsis(Ellipsis e, OffsetLength range) {
		super("Unsupported use of ellipsis", range);
	}
}
