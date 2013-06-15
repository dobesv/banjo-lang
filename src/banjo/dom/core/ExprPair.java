package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.Precedence;

/**
 * Sequencing operation.  Any "let" expressions (or other environmental changes) in the action
 * expression are visible during the evaluation of "result".  The evaluation of "result" determines
 * the resulting value of the expression.
 */
public class ExprPair extends AbstractCoreExpr implements CoreExpr {
	private final CoreExpr action;
	private final CoreExpr result;

	public ExprPair(int sourceLength, CoreExpr action, CoreExpr result) {
		super(sourceLength, action.hashCode() ^ result.hashCode());
		this.action = action;
		this.result = result;
	}

	@Override
	public void toSource(StringBuffer sb) {
		this.action.toSource(sb, Precedence.SEMICOLON);
		sb.append("; ");
		this.result.toSource(sb, Precedence.SEMICOLON);
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.SEMICOLON;
	}

	@Override
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.exprPair(this);
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (!(obj instanceof ExprPair))
			return false;
		final ExprPair other = (ExprPair) obj;
		return this.action.equals(other.action) && this.result.equals(other.result);
	}

	public CoreExpr getAction() {
		return this.action;
	}

	public CoreExpr getResult() {
		return this.result;
	}
}
