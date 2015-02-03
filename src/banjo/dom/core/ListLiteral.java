package banjo.dom.core;

import banjo.dom.Expr;
import banjo.dom.source.Precedence;
import banjo.dom.token.Identifier;
import banjo.parser.util.ListUtil;
import banjo.parser.util.SourceFileRange;
import fj.data.List;

public class ListLiteral extends AbstractCoreExpr implements CoreExpr {
	public static final ListLiteral EMPTY_LIST = new ListLiteral(List.nil(), List.nil());
	private final List<CoreExpr> elements;

	public ListLiteral(List<SourceFileRange> ranges, List<CoreExpr> elements) {
		super(elements.hashCode()+ranges.hashCode(), ranges);
		this.elements = elements;
	}

	public ListLiteral(List<CoreExpr> elements) {
		this(SourceFileRange.EMPTY_LIST, elements);
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
			elt.toSource(sb, Precedence.COMMA);
		}
		sb.append(']');
	}

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.listLiteral(this);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
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
		if(o == null) return -1;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final ListLiteral other = (ListLiteral) o;
			if(cmp == 0) cmp = ListUtil.compare(this.elements, other.elements, CoreExpr.ORD);
			if(cmp == 0) cmp = super.compareTo(other);
		}
		return cmp;
	}

	@Override
	public <T> T acceptVisitor(final CoreExprAlgebra<T> visitor) {
		return visitor.listLiteral(getSourceFileRanges(), elements.<T>map(a -> a.acceptVisitor(visitor)));
	}

	public CoreExpr toConstructionExpression() {
		CoreExpr result = Identifier.EMPTY_LIST;
		for(CoreExpr elt : elements) {
			result = new Call(Identifier.DATA, new Identifier("list"), ObjectLiteral.selector("nonempty", elt, result));
		}
		return result;
	}

}
