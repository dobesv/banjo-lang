package banjo.dom.source;

import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

import banjo.parser.errors.BanjoParseException;

public class BadSourceExpr extends AbstractCompositeSourceExpr {
	private final BanjoParseException error;

	public BadSourceExpr(List<SourceNode> children, BanjoParseException err) {
		super(children);
		this.error = err;
	}

	@Override
	@Nullable
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.visitBadSourceExpr(this);
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.lowest();
	}

	public BanjoParseException getError() {
		return error;
	}

}
