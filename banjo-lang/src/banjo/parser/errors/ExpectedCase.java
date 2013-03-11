package banjo.parser.errors;

import banjo.parser.ast.Expr;
import banjo.parser.util.FileRange;

public class ExpectedCase extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public ExpectedCase(String message, FileRange range) {
		super(message, range);
	}
	
	public ExpectedCase(Expr butGot) {
		super("Expected a condition case; got '"+butGot+"'", butGot.getFileRange());
	}
}