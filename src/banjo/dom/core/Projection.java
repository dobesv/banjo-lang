package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExpr;
import banjo.dom.token.NumberLiteral;

/**
 * A projection of the form <code>object . expr</code> evaluates <code>expr</code> such
 * that identifiers in <code>expr</code> will be resolved as fields of <code>object</code>
 * rather than as variables in the surrounding environment.
 * 
 * The simplest case for this is direct field access - <code>foo.bar</code> accesses field
 * <code>bar</code> in the object referenced by <code>foo</code>.
 */
public class Projection extends AbstractCoreExpr implements CoreExpr {

	private final CoreExpr object;
	private final CoreExpr body;

	public Projection(SourceExpr sourceExpr, CoreExpr object, CoreExpr body) {
		this(sourceExpr.getSourceLength(), object, body);
	}

	public Projection(int sourceLength, CoreExpr object, CoreExpr body) {
		super(sourceLength, body.hashCode() + object.hashCode());
		this.object = object;
		this.body = body;
	}

	public CoreExpr getObject() {
		return this.object;
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.SUFFIX;
	}

	@Override
	public void toSource(StringBuffer sb) {
		// A number literal shouldn't be followed by this dot.  Any other expression will be parenthesized automatically
		final boolean num = NumberLiteral.isNumberLiteral(this.object);
		if(num) sb.append('(');
		this.object.toSource(sb, Precedence.SUFFIX);
		if(num) sb.append(')');
		sb.append('.');
		this.body.toSource(sb);
	}

	public CoreExpr getBody() {
		return this.body;
	}

	@Override
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.projection(this);
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (!(obj instanceof Projection))
			return false;
		final Projection other = (Projection) obj;
		if (!this.body.equals(other.body))
			return false;
		if (!this.object.equals(other.object))
			return false;
		return true;
	}

}
