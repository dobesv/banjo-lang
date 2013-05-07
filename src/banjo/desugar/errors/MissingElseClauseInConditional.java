package banjo.desugar.errors;

import banjo.parser.errors.Problem;

public class MissingElseClauseInConditional extends Problem {
	public MissingElseClauseInConditional(int sourceOffset, int sourceLength) {
		super("Missing else clause in conditional", sourceOffset, sourceLength);
	}

	private static final long serialVersionUID = 1L;

}
