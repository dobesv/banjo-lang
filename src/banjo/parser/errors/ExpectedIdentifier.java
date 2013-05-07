package banjo.parser.errors;

import banjo.dom.source.SourceExpr;

public class ExpectedIdentifier extends Problem {
	private static final long serialVersionUID = 1L;

	public ExpectedIdentifier(int sourceOffset, int sourceLength) {
		super("Expected identifier here", sourceOffset, sourceLength);
	}

	public ExpectedIdentifier(SourceExpr gotInstead, int offset, int length) {
		super("Expected identifier; got '"+gotInstead+"'", offset, length);
	}
}