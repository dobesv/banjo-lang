package banjo.desugar.errors;

import banjo.parser.errors.Problem;

public class ExpectedConditional extends Problem {
	private static final long serialVersionUID = 1L;

	public ExpectedConditional(int sourceOffset, int sourceLength) {
		super("Expected conditional", sourceOffset, sourceLength);
	}
}
