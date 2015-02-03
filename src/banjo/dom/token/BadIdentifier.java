package banjo.dom.token;

import banjo.dom.BadExpr;
import banjo.dom.Expr;
import banjo.dom.core.AbstractCoreExpr;
import banjo.dom.core.CoreExprAlgebra;
import banjo.dom.core.CoreExprVisitor;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExpr;
import banjo.dom.source.SourceExprAlgebra;
import banjo.dom.source.SourceExprVisitor;
import banjo.parser.util.SourceFileRange;
import fj.data.List;

public class BadIdentifier extends AbstractCoreExpr implements BadExpr {
	public final String message;
	public final String originalSource;

	public BadIdentifier(List<SourceFileRange> ranges, String message, String originalSource) {
		super(message.hashCode() + originalSource.hashCode(), ranges);
		this.message = message;
		this.originalSource = originalSource;
	}

	public BadIdentifier(SourceExpr source) {
		this(source.getSourceFileRanges(), "Expected identifier; got "+source.toSource(), source.toSource());
	}

	public BadIdentifier(SourceFileRange sfr, String message, String originalSource) {
		this(List.single(sfr), message, originalSource);
    }

	@Override
	public void toSource(StringBuffer sb) {
		Identifier.toSource(originalSource, sb);
	}

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.badIdentifier(this);
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}

	@Override
	public int compareTo(Expr o) {
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
	public boolean equals(Object obj) {
		if(obj == this) return true;
		if(obj == null || !(obj instanceof BadIdentifier)) return false;
		if(!super.equals(obj)) return false;
		final BadIdentifier x = (BadIdentifier) obj;
		return x.message.equals(this.message) && x.originalSource.equals(this.originalSource);
	}

	@Override
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.badIdentifier(this);
	}

	@Override
	public String getMessage() {
		return this.message;
	}

	@Override
	public List<BadExpr> getProblems() {
		return List.single(this);
	}

	@Override
	public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
		return visitor.badExpr(getSourceFileRanges(), message);
	}

	@Override
	public <T> T acceptVisitor(SourceExprAlgebra<T> visitor) {
		return visitor.badExpr(getSourceFileRanges(), message);
	}

	@Override
	public List<String> getParts() {
		return List.single(originalSource);
	}

	@Override
	public Key withoutPrefix() {
	    return this;
	}
}
