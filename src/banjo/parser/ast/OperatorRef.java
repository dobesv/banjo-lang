package banjo.parser.ast;

import banjo.parser.util.FileRange;

public class OperatorRef extends Expr {

	private final String op;

	public OperatorRef(FileRange range, String op) {
		super(range);
		this.op = op;
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append(this.getOp());
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}

	public String getOp() {
		return op;
	}

}
