package banjo.desugar.errors;

import banjo.dom.BinaryOp;
import banjo.parser.errors.BanjoParseException;
import banjo.parser.util.FileRange;

public class MixedSemicolonAndComma extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public MixedSemicolonAndComma(String message, FileRange range) {
		super(message, range);
	}

	public MixedSemicolonAndComma(BinaryOp bop) {
		this("List mixes semicolons and commas", bop.getFileRange());
	}

}
