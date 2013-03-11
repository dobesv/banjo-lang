package banjo.parser.errors;

import banjo.parser.util.FileRange;

public class ExpectedExpression extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public ExpectedExpression(FileRange fileRange) {
		super("Expected expression", fileRange);
	}
	public ExpectedExpression(FileRange fileRange, String butGot) {
		super("Expected expression here; found '"+butGot+"'", fileRange);
	}
}