package banjo.expr;

import banjo.expr.source.Precedence;
import banjo.expr.util.OrdUtil;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.data.Set;



public interface Expr {
	public static final Ord<Expr> EXPR_RANGES_ORD = SourceFileRange.SET_ORD.contramap(x -> x.getSourceFileRanges());
	public static final Ord<Object> CLASS_NAME_ORD = Ord.stringOrd.contramap(x -> x.getClass().getName());

	public void toSource(StringBuffer sb);

	public void toSource(StringBuffer sb, Precedence outerPrec);

	public String toSource(Precedence prec);

	public String toSource();

	public Precedence getPrecedence();

	public Set<SourceFileRange> getSourceFileRanges();

	public static <A extends Expr> Ord<A> ordWithFileRanges(Ord<A> subclassOrd) {
		return OrdUtil.chain(EXPR_RANGES_ORD, subclassOrd);
	}

}