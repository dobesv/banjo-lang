package banjo.desugar.errors;

import banjo.parser.errors.BanjoParseException;
import banjo.parser.util.OffsetLength;

public class MultipleElseClausesInConditional extends BanjoParseException {
	public MultipleElseClausesInConditional(OffsetLength range) {
		super("Multiple else clauses in conditional", range);
	}

	private static final long serialVersionUID = 1L;



}
