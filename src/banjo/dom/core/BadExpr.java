package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExpr;
import banjo.dom.token.StringLiteral;
import banjo.parser.errors.BanjoParseException;

public class BadExpr extends AbstractCoreExpr implements CoreExpr {
	private final BanjoParseException error;
	
	public BadExpr(SourceExpr expr, BanjoParseException error) {
		super(expr);
		this.error = error;
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append("fail(");
		StringLiteral.toSource(error.toString(), sb);
		sb.append(")");
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.SUFFIX;
	}

	@Override
	@Nullable
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.visitBadExpr(this);
	}

}
