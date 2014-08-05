package banjo.dom.source;

import static banjo.parser.util.Check.nonNull;

import java.util.Comparator;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.BadExpr;
import banjo.dom.Expr;
import fj.F;
import fj.Ord;
import fj.Ordering;
import fj.data.List;
import fj.data.Option;

/**
 * An expression as it appeared in the source code, without any desugaring applied.
 */
public interface SourceExpr extends Expr, SourceNode {

	public static final Option<Integer> NOT_A_CHILD = nonNull(Option.<Integer>none());

	<T> T acceptVisitor(SourceExprVisitor<T> visitor);
	<T> T acceptVisitor(SourceExprAlgebra<T> visitor);

	public static final Ord<SourceExpr> ORD = nonNull(Ord.ord(new F<SourceExpr, F<SourceExpr, Ordering>>() {
		@Override
		public F<SourceExpr, Ordering> f(final @Nullable SourceExpr a1) {
			return new F<SourceExpr, Ordering>() {
				@SuppressWarnings("null")
				@Override
				public Ordering f(final @Nullable SourceExpr a2) {
					final int x = a1 == a2 ? 0 : a1 == null ? 1 : a2 == null ? -1 : a1.compareTo(a2);
					return x < 0 ? Ordering.LT : x == 0 ? Ordering.EQ : Ordering.GT;
				}
			};
		}
	}));

	public static final Comparator<SourceExpr> COMPARATOR = new Comparator<SourceExpr>() {
		@Override
		public int compare(@Nullable SourceExpr o1, @Nullable SourceExpr o2) {
			return o1==o2?0:o1==null?1:o2==null?-1:o1.compareTo(o2);
		}
	};

	String toFullyParenthesizedSource();

	void toFullyParenthesizedSource(StringBuffer sb);

	List<BadExpr> getProblems();

}
