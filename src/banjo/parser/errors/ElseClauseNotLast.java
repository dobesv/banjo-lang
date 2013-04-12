package banjo.parser.errors;

import banjo.dom.Expr;

public class ElseClauseNotLast extends BanjoParseException {
	public ElseClauseNotLast(Expr duplicateElseClause) {
		super("Else clause must be last clause in conditional", duplicateElseClause.getFileRange());
	}

	private static final long serialVersionUID = 1L;

	
}
