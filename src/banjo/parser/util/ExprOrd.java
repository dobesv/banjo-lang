package banjo.parser.util;

import static banjo.parser.util.Check.nonNull;
import banjo.dom.Expr;
import fj.F;
import fj.Ord;
import fj.Ordering;

public class ExprOrd {

	public static <T extends Expr> Ord<T> exprOrd() {
		return nonNull(Ord.ord(new F<T, F<T, Ordering>>() {
			@Override
			public F<T, Ordering> f(final T a1) {
				return new F<T, Ordering>() {
					@SuppressWarnings("null")
					@Override
					public Ordering f(final T a2) {
						final int x = a1.compareTo(a2);
						return x < 0 ? Ordering.LT : x == 0 ? Ordering.EQ : Ordering.GT;
					}
				};
			}
		}));
	}

}
