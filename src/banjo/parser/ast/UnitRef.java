package banjo.parser.ast;

import banjo.parser.util.FileRange;

public class UnitRef extends Expr {

	private final ParenType parenType;

	public UnitRef(FileRange range) {
		super(range);
		this.parenType = null;
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
		sb.append(parenType.getStartChar()).append(parenType.getEndChar());
	}

}
