package banjo.desugar.errors;

import banjo.parser.errors.Problem;

public class MultipleElseClausesInConditional extends Problem {
	public MultipleElseClausesInConditional(int sourceOffset, int sourceLength) {
		super("Multiple else clauses in conditional", sourceOffset, sourceLength);
	}

	private static final long serialVersionUID = 1L;



}
