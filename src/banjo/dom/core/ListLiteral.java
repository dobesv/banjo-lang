package banjo.dom.core;

import static banjo.parser.util.Check.nonNull;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExpr;

public class ListLiteral extends AbstractCoreExpr implements CoreExpr {

	private final List<CoreExpr> elements;

	public ListLiteral(SourceExpr sourceExpr, List<CoreExpr> elements) {
		super(sourceExpr, elements.hashCode());
		this.elements = nonNull(Collections.unmodifiableList(elements));
	}

	public Collection<CoreExpr> getElements() {
		return this.elements;
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append('[');
		boolean first = true;
		for(final CoreExpr elt : this.elements) {
			if(first) first = false;
			else sb.append(", ");
			elt.toSource(sb);
		}
		sb.append(']');
	}

	@Override
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.listLiteral(this);
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (!(obj instanceof ListLiteral))
			return false;
		final ListLiteral other = (ListLiteral) obj;
		if (!this.elements.equals(other.elements))
			return false;
		return true;
	}

}
