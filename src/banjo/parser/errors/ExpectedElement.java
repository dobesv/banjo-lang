package banjo.parser.errors;


public class ExpectedElement extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public ExpectedElement(int sourceOffset, int sourceLength) {
		super("Expected comma, newline, or ']'", sourceOffset, sourceLength);
	}
}