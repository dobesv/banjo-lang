package banjo.parser.errors;

import banjo.parser.util.FileRange;

public class EmptyBacktick extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public EmptyBacktick(FileRange range) {
		super("Backtick should be following by an identifier or operator", range);
	}

}
