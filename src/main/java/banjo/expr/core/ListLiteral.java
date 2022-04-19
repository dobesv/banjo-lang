package banjo.expr.core;

import banjo.expr.source.BinaryOp;
import banjo.expr.source.Operator;
import banjo.expr.source.Precedence;
import banjo.expr.source.SourceExpr;
import banjo.expr.source.UnaryOp;
import banjo.expr.util.ListUtil;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.data.List;
import fj.data.Set;

public class ListLiteral extends AbstractCoreExpr implements CoreExpr {
	public static final Ord<ListLiteral> ORD = CoreExprOrd.LIST_ORD.contramap(x -> x.elements);

	public static final ListLiteral EMPTY_LIST = new ListLiteral(List.nil());
	public final List<CoreExpr> elements;

	public ListLiteral(Set<SourceFileRange> ranges, List<CoreExpr> elements) {
		super(elements.hashCode()+ranges.hashCode(), ranges);
		this.elements = elements;
	}

	public ListLiteral(List<CoreExpr> elements) {
		this(SourceFileRange.EMPTY_SET, elements);
	}

	public List<CoreExpr> getElements() {
		return this.elements;
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.listLiteral(this);
	}

	@Override
	public <T> T acceptVisitor(final CoreExprAlgebra<T> visitor) {
		return visitor.listLiteral(getRanges(), elements.<T>map(a -> a.acceptVisitor(visitor)));
	}

	@Override
	public String toString() {
		if(elements.isEmpty())
			return "[]";
		if(elements.length() > 5) {
			return "["+ListUtil.insertCommas(elements.take(5))+", ...]";
		} else {
			return "["+ListUtil.insertCommas(elements)+"]";
		}
	}

}
