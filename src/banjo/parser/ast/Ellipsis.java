package banjo.parser.ast;

import banjo.parser.util.FileRange;

public class Ellipsis extends Expr {

	public Ellipsis(FileRange range) {
		super(range);
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append("...");
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}

}
