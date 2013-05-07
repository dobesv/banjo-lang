package banjo.dom.source;

import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

import banjo.parser.errors.Problem;

public class BadSourceExpr extends AbstractCompositeSourceExpr {
	private final Problem error;

	public BadSourceExpr(List<SourceNode> children, Problem err) {
		super(children);
		this.error = err;
	}

	@Override
	@Nullable
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.badSourceExpr(this);
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.lowest();
	}

	public Problem getError() {
		return this.error;
	}

}
