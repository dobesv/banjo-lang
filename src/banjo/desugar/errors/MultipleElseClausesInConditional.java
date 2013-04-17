package banjo.desugar.errors;

import banjo.dom.Expr;
import banjo.parser.errors.BanjoParseException;

public class MultipleElseClausesInConditional extends BanjoParseException {
	public MultipleElseClausesInConditional(Expr elseClause) {
		super("Multiple else clauses in conditional", elseClause.getFileRange());
	}

	private static final long serialVersionUID = 1L;
	
	

}
