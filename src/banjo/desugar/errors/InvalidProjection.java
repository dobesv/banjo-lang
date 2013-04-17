package banjo.desugar.errors;

import banjo.dom.Expr;
import banjo.parser.errors.BanjoParseException;

public class InvalidProjection extends BanjoParseException {
	private static final long serialVersionUID = 1L;
	
	public InvalidProjection(Expr right) {
		super("Invalid projection", right.getFileRange());
	}
}
