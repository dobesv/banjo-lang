package banjo.parser.errors;

import banjo.dom.ParenType;
import banjo.parser.util.FileRange;

public class UnexpectedCloseParen extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public UnexpectedCloseParen(FileRange range) {
		super("Unexpected ')'", range);
	}

	public UnexpectedCloseParen(FileRange range, ParenType type) {
		super("Unexpected '"+(char)type.getEndChar()+"'", range);
	}
	
}