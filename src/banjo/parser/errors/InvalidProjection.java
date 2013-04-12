package banjo.parser.errors;

import banjo.dom.Expr;

public class InvalidProjection extends BanjoParseException {
	private static final long serialVersionUID = 1L;
	
	public InvalidProjection(Expr right) {
		super("Invalid projection", right.getFileRange());
	}
}
