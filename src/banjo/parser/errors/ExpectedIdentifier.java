package banjo.parser.errors;

import banjo.dom.Expr;

public class ExpectedIdentifier extends Problem {
	private static final long serialVersionUID = 1L;

	public ExpectedIdentifier(int sourceOffset, int sourceLength) {
		super("Expected identifier here", sourceOffset, sourceLength);
	}

	public ExpectedIdentifier(Expr gotInstead, int offset, int length) {
		super("Expected identifier; got '"+gotInstead+"'", offset, length);
	}
}