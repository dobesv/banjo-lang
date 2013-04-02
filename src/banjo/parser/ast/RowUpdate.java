package banjo.parser.ast;

import banjo.parser.util.FileRange;

public class RowUpdate extends Expr {
	private final Expr base;
	private final ObjectLiteral updates;
	
	public RowUpdate(FileRange range, Expr base, ObjectLiteral updates) {
		super(range);
		this.base = base;
		this.updates = updates;
	}

	@Override
	public void toSource(StringBuffer sb) {
		base.toSource(sb, getPrecedence());
		sb.append('.');
		updates.toSource(sb, getPrecedence());
	}

	@Override
	public Precedence getPrecedence() {
		return BinaryOperator.PROJECTION.getPrecedence();
	}

}
