package banjo.dom.source;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.Comparator;

import banjo.dom.BadExpr;
import banjo.dom.Expr;
import banjo.parser.SourceCodeParser;
import fj.F;
import fj.Ord;
import fj.Ordering;
import fj.data.List;

/**
 * An expression as it appeared in the source code, without any desugaring applied.
 */
public interface SourceExpr extends Expr, SourceNode {

	<T> T acceptVisitor(SourceExprVisitor<T> visitor);
	<T> T acceptVisitor(SourceExprAlgebra<T> visitor);

	public static final Ord<SourceExpr> ORD = Ord.ord(new F<SourceExpr, F<SourceExpr, Ordering>>() {
		@Override
		public F<SourceExpr, Ordering> f(final SourceExpr a1) {
			return new F<SourceExpr, Ordering>() {
				
				@Override
				public Ordering f(final SourceExpr a2) {
					final int x = a1 == a2 ? 0 : a1 == null ? 1 : a2 == null ? -1 : a1.compareTo(a2);
					return x < 0 ? Ordering.LT : x == 0 ? Ordering.EQ : Ordering.GT;
				}
			};
		}
	});

	public static final Comparator<SourceExpr> COMPARATOR = new Comparator<SourceExpr>() {
		@Override
		public int compare(SourceExpr o1, SourceExpr o2) {
			return o1==o2?0:o1==null?1:o2==null?-1:o1.compareTo(o2);
		}
	};

	String toFullyParenthesizedSource();

	void toFullyParenthesizedSource(StringBuffer sb);

	List<BadExpr> getProblems();

	public static SourceExpr fromString(String src) {
		try {
			return new SourceCodeParser().parse(src);
		} catch (IOException e) {
			throw new UncheckedIOException(e);
		}
	}

}
