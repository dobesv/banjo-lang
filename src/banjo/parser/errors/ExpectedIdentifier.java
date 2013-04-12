package banjo.parser.errors;

import banjo.dom.Expr;
import banjo.parser.util.FileRange;

public class ExpectedIdentifier extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public ExpectedIdentifier(FileRange fileRange) {
		super("Expected identifier here", fileRange);
	}

	public ExpectedIdentifier(Expr gotInstead) {
		super("Expected identifier; got '"+gotInstead+"'", gotInstead.getFileRange());
	}
}