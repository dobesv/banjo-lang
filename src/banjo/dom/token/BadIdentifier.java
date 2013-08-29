package banjo.dom.token;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.BadExpr;
import banjo.dom.Expr;
import banjo.dom.core.AbstractCoreExpr;
import banjo.dom.core.CoreExprVisitor;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExpr;
import banjo.dom.source.SourceExprVisitor;

public class BadIdentifier extends AbstractCoreExpr implements Key, BadExpr {
	private final String message;
	final String originalSource;

	public BadIdentifier(String message, String originalSource) {
		super(message.hashCode() + originalSource.hashCode());
		this.message = message;
		this.originalSource = originalSource;
	}

	public BadIdentifier(SourceExpr source) {
		this("Expected identifier; got "+source.toSource(), source.toSource());
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append(this.originalSource);
	}

	@Override
	@Nullable
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.badIdentifier(this);
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}

	@Override
	public int compareTo(Expr o) {
		if(this == o)
			return 0;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final BadIdentifier other = (BadIdentifier) o;
			if(cmp == 0) cmp = this.getMessage().compareTo(other.getMessage());
			if(cmp == 0) cmp = this.originalSource.compareTo(other.originalSource);
		}
		return cmp;
	}

	@Override
	@Nullable
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		throw new Error("Not a source expression, really.");
	}

	@Override
	public String getKeyString() {
		return this.originalSource;
	}

	@Override
	public String getMessage() {
		return this.message;
	}

}
