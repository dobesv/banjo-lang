package banjo.parser.ast;

import banjo.parser.util.FileRange;

public class UnitRef extends BaseExpr {

	private final ParenType parenType;

	public UnitRef(FileRange range) {
		super(range);
		this.parenType = ParenType.BRACES;
	}

	public UnitRef(FileRange fileRange, ParenType parenType) {
		super(fileRange);
		this.parenType = parenType;
	}

	public ParenType getParenType() {
		return parenType;
	}
	
	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}
	
	@Override
	public void toSource(StringBuffer sb) {
		sb.appendCodePoint(parenType.getStartChar()).appendCodePoint(parenType.getEndChar());
	}

}
