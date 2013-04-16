package banjo.parser.errors;

import banjo.dom.Expr;

public class MultipleElseClausesInConditional extends BanjoParseException {
	public MultipleElseClausesInConditional(Expr elseClause) {
		super("Multiple else clauses in conditional", elseClause.getFileRange());
	}

	private static final long serialVersionUID = 1L;
	
	

}