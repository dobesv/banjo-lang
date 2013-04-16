package banjo.dom;

import banjo.parser.util.FileRange;
import fj.data.Option;

public abstract class AbstractOp extends AbstractExpr {
	protected final Operator operator;
	protected final OperatorRef opToken;
	protected final Option<OperatorRef> closeParenToken;
	
	public AbstractOp(FileRange range, Operator operator, OperatorRef opToken, Option<OperatorRef> closeParenToken) {
		super(range);
		this.operator = operator;
		this.opToken = opToken;
		this.closeParenToken = closeParenToken;
	}

	public Operator getOperator() {
		return operator;
	}

	public OperatorRef getOpToken() {
		return opToken;
	}

	public Option<OperatorRef> getCloseParenToken() {
		return closeParenToken;
	}
	
	@Override
	public Precedence getPrecedence() {
		return operator.getPrecedence();
	}

	

}
