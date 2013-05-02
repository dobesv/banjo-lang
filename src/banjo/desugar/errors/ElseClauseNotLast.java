package banjo.desugar.errors;

import banjo.parser.errors.BanjoParseException;
import banjo.parser.util.OffsetLength;

public class ElseClauseNotLast extends BanjoParseException {
	public ElseClauseNotLast(OffsetLength range) {
		super("Else clause must be last clause in cond.itional", range);
	}

	private static final long serialVersionUID = 1L;


}
