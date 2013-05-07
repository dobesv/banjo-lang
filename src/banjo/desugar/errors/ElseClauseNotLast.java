package banjo.desugar.errors;

import banjo.parser.errors.Problem;

public class ElseClauseNotLast extends Problem {
	public ElseClauseNotLast(int sourceOffset, int sourceLength) {
		super("Else clause must be last clause in a conditional", sourceOffset, sourceLength);
	}

	private static final long serialVersionUID = 1L;


}
