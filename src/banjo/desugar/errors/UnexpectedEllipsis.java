package banjo.desugar.errors;

import banjo.dom.token.Ellipsis;
import banjo.parser.errors.Problem;

public class UnexpectedEllipsis extends Problem {
	private static final long serialVersionUID = 1L;

	public UnexpectedEllipsis(Ellipsis e, int sourceOffset) {
		super("Unsupported use of ellipsis", sourceOffset, e.getSourceLength());
	}
}
