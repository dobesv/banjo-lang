package banjo.parser.errors;

import banjo.parser.util.OffsetLength;

public class ExpectedExpression extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public ExpectedExpression(OffsetLength fileRange) {
		super("Expected expression", fileRange);
	}
	public ExpectedExpression(OffsetLength offsetLength, String butGot) {
		super("Expected expression here; found '"+butGot+"'", offsetLength);
	}
}