package banjo.parser.errors;

import banjo.dom.source.SourceExpr;
import banjo.parser.util.OffsetLength;

public class ExpectedIdentifier extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public ExpectedIdentifier(int sourceOffset, int sourceLength) {
		super("Expected identifier here", sourceOffset, sourceLength);
	}

	public ExpectedIdentifier(SourceExpr gotInstead, OffsetLength sourceOffsetLength) {
		super("Expected identifier; got '"+gotInstead+"'", sourceOffsetLength);
	}

	public ExpectedIdentifier(OffsetLength sourceOffsetLength) {
		super("Expected identifier", sourceOffsetLength);
	}
}