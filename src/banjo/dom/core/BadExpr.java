package banjo.dom.core;

import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExpr;
import banjo.dom.token.StringLiteral;
import banjo.parser.errors.Problem;

public class BadExpr extends AbstractCoreExpr implements CoreExpr {
	private final Problem problem;

	public BadExpr(SourceExpr expr, Problem problem) {
		super(expr, problem.getMessage().hashCode());
		this.problem = problem;
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append("fail(");
		StringLiteral.toSource(nonNull(this.problem.toString()), sb);
		sb.append(")");
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.SUFFIX;
	}

	@Override
	@Nullable
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.badExpr(this);
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (!(obj instanceof BadExpr))
			return false;
		final BadExpr other = (BadExpr) obj;
		if (!this.problem.equals(other.problem))
			return false;
		return true;
	}

}
