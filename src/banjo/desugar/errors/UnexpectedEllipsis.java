package banjo.desugar.errors;

import banjo.dom.Ellipsis;
import banjo.parser.errors.BanjoParseException;

public class UnexpectedEllipsis extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public UnexpectedEllipsis(Ellipsis e) {
		super("Unsupported use of ellipsis", e.getFileRange());
	}
}
