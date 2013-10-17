package banjo.parser.util;

import fj.F;
import fj.Ord;
import fj.Ordering;

public class ReverseOrd {
	@SuppressWarnings("null")
	public static <T> Ord<T> reverseOrd(final Ord<T> forwardOrd) {

		return Ord.ord(new F<T,F<T,Ordering>>() {

			@Override
			public F<T, Ordering> f(final T a) {
				return new F<T, Ordering>() {
					@Override
					public Ordering f(T b) {
						final Ordering cmp = forwardOrd.compare(a, b);
						switch(cmp) {
						default: return cmp;
						case GT: return Ordering.LT;
						case LT: return Ordering.GT;
						}
					}
				};
			}

		});
	}
}
