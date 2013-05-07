package banjo.desugar.errors;

import banjo.dom.Expr;
import banjo.parser.errors.Problem;
import banjo.parser.util.FileRange;
import banjo.parser.util.OffsetLength;

public class ExpectedCase extends Problem {
	private static final long serialVersionUID = 1L;

	public ExpectedCase(String message, FileRange range) {
		super(message, range);
	}

	public ExpectedCase(Expr butGot, OffsetLength range) {
		super("Expected a condition case; got '"+butGot+"'", range);
	}
}