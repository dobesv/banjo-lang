package banjo.dom.core;

import banjo.dom.source.Precedence;
import banjo.dom.token.Identifier;
import banjo.parser.util.SourceFileRange;
import fj.Ord;
import fj.data.List;

public class ListLiteral extends AbstractCoreExpr implements CoreExpr {
	public static final Ord<ListLiteral> ORD = CoreExpr.listOfCoreExprOrd.comap(x -> x.elements);

	public static final ListLiteral EMPTY_LIST = new ListLiteral(List.nil(), List.nil());
	public final List<CoreExpr> elements;

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
	public <T> T acceptVisitor(final CoreExprAlgebra<T> visitor) {
		return visitor.listLiteral(getSourceFileRanges(), elements.<T>map(a -> a.acceptVisitor(visitor)));
	}

	public CoreExpr toConstructionExpression() {
		CoreExpr result = Identifier.EMPTY_LIST;
		for(CoreExpr elt : elements) {
			result = Call.slot(Identifier.DATA, "list", List.single(FunctionLiteral.selector("nonempty", elt, result)));
		}
		return result;
	}

	@Override
	public String toString() {
		if(elements.length() > 5) {
			return "["+elements.take(5)+", ...]";
		} else {
			return "["+elements+"]";
		}
	}

}
