package banjo.parser.ast;

import banjo.parser.util.FileRange;

public class CondCase extends BaseExpr {

	private final Expr condition;
	private final Expr value;

	public CondCase(FileRange range, Expr condition, Expr value) {
		super(range);
		this.condition = condition;
		this.value = value;
	}

	@Override
	public void toSource(StringBuffer sb) {
		condition.toSource(sb, Precedence.COND);
		sb.append(' ');
		sb.append(BinaryOperator.COND.getOp());
		sb.append(' ');
		value.toSource(sb, Precedence.COND);
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.COND;
	}

	public Expr getCondition() {
		return condition;
	}

	public Expr getValue() {
		return value;
	}

}
