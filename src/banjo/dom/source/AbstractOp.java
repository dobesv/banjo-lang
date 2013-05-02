package banjo.dom.source;

import java.util.List;

public abstract class AbstractOp extends AbstractCompositeSourceExpr {
	protected final Operator operator;

	public AbstractOp(List<SourceNode> children, Operator operator) {
		super(children);
		this.operator = operator;
	}

	public Operator getOperator() {
		return this.operator;
	}

	@Override
	public Precedence getPrecedence() {
		return this.operator.getPrecedence();
	}
}
