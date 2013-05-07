package banjo.desugar.errors;

import banjo.parser.errors.Problem;

public class InvalidProjection extends Problem {
	private static final long serialVersionUID = 1L;

	public InvalidProjection(String src, int offset, int length) {
		super("Invalid projection "+src, offset, length);
	}
}
