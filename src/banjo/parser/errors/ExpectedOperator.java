package banjo.parser.errors;

import banjo.parser.ast.BaseExpr;
import banjo.parser.ast.Precedence;
import banjo.parser.util.FileRange;

public class ExpectedOperator extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public ExpectedOperator(FileRange fileRange) {
		super("Expected operator", fileRange);
	}

}