package banjo.desugar.errors;

import banjo.dom.source.BinaryOp;
import banjo.parser.errors.Problem;
import banjo.parser.util.FileRange;
import banjo.parser.util.OffsetLength;

public class MixedSemicolonAndComma extends Problem {
	private static final long serialVersionUID = 1L;

	public MixedSemicolonAndComma(String message, FileRange range) {
		super(message, range);
	}

	public MixedSemicolonAndComma(BinaryOp bop, OffsetLength range) {
		super("List mixes semicolons and commas", range);
	}

}
