package banjo.dom.token;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.BadExpr;
import banjo.dom.Expr;
import banjo.dom.core.AbstractCoreExpr;
import banjo.dom.core.CoreExprVisitor;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExpr;
import banjo.dom.source.SourceExprVisitor;
import banjo.parser.util.SourceFileRange;
import fj.data.List;

public class BadIdentifier extends AbstractCoreExpr implements Key, BadExpr {
	private final String message;
	final String originalSource;

	public BadIdentifier(SourceFileRange sfr, String message, String originalSource) {
		super(message.hashCode() + originalSource.hashCode(), sfr);
		this.message = message;
		this.originalSource = originalSource;
	}

	public BadIdentifier(SourceExpr source) {
		this(source.getSourceFileRange(), "Expected identifier; got "+source.toSource(), source.toSource());
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
	public int compareTo(@Nullable Expr o) {
		if(o == null) return -1;
		if(this == o)
			return 0;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final BadIdentifier other = (BadIdentifier) o;
			if(cmp == 0) cmp = this.getMessage().compareTo(other.getMessage());
			if(cmp == 0) cmp = this.originalSource.compareTo(other.originalSource);
			if(cmp == 0) cmp = super.compareTo(o);
		}
		return cmp;
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if(obj == this) return true;
		if(obj == null || !(obj instanceof BadIdentifier)) return false;
		if(!super.equals(obj)) return false;
		final BadIdentifier x = (BadIdentifier) obj;
		return x.message.equals(this.message) && x.originalSource.equals(this.originalSource);
	}

	@Override
	@Nullable
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.badIdentifier(this);
	}

	@Override
	public String getKeyString() {
		return this.originalSource;
	}

	@Override
	public String getMessage() {
		return this.message;
	}

	@Override
	public List<BadExpr> getProblems() {
		return List.<BadExpr>single(this);
	}
}
