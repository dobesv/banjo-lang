package banjo.desugar.errors;

import banjo.parser.errors.Problem;

public class ExpectedField extends Problem {
	private static final long serialVersionUID = 1L;

	public ExpectedField(int offset, int length) {
		super("Expected key : value pair", offset, length);
	}
}