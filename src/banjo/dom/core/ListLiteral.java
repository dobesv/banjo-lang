package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Expr;
import banjo.dom.source.Precedence;
import banjo.parser.util.ListUtil;
import fj.data.List;

public class ListLiteral extends AbstractCoreExpr implements CoreExpr {

	private final List<CoreExpr> elements;

	public ListLiteral(List<CoreExpr> elements) {
		super(elements.hashCode());
		this.elements = elements;
	}

	public List<CoreExpr> getElements() {
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

	@Override
	public int compareTo(Expr o) {
		if(this == o)
			return 0;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final ListLiteral other = (ListLiteral) o;
			if(cmp == 0) cmp = ListUtil.<Expr>compare(this.elements, other.elements);
		}
		return cmp;
	}

}
