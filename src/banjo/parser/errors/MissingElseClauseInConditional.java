package banjo.parser.errors;

import banjo.parser.util.FileRange;

public class MissingElseClauseInConditional extends BanjoParseException {
	public MissingElseClauseInConditional(FileRange range) {
		super("Missing else clause in conditional", range);
	}

	private static final long serialVersionUID = 1L;

}
