package banjo.parser.errors;


public class ExpectedExpression extends Problem {
	private static final long serialVersionUID = 1L;

	public ExpectedExpression(int sourceOffset, int sourceLength) {
		super("Expected expression", sourceOffset, sourceLength);
	}
	public ExpectedExpression(int sourceOffset, int sourceLength, String butGot) {
		super("Expected expression here; found '"+butGot+"'", sourceOffset, sourceLength);
	}
}