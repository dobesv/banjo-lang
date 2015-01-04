package banjo.parser.util;

import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Expr;
import fj.F;
import fj.Ord;
import fj.Ordering;

public class ExprOrd {

	public static <T extends Expr> Ord<@NonNull T> exprOrd() {
		return Ord.ord(new F<T, F<T, Ordering>>() {
			@Override
			public F<T, Ordering> f(final @Nullable T a1) {
				return new F<T, Ordering>() {
					@SuppressWarnings("null")
					@Override
					public Ordering f(final @Nullable T a2) {
						final int x = a1 == a2 ? 0 : a1 == null ? 1 : a2 == null ? -1 : a1.compareTo(a2);
						return x < 0 ? Ordering.LT : x == 0 ? Ordering.EQ : Ordering.GT;
					}
				};
			}
		});
	}

}
