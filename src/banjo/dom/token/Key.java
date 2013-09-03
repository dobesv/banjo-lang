package banjo.dom.token;

import static banjo.parser.util.Check.nonNull;
import banjo.dom.core.CoreExpr;
import fj.F;
import fj.Ord;
import fj.Ordering;


public interface Key extends Atom, CoreExpr {
	public String getKeyString();

	public static final Ord<Key> ORD = nonNull(Ord.ord(new F<Key, F<Key, Ordering>>() {
		@Override
		public F<Key, Ordering> f(final Key a1) {
			return new F<Key, Ordering>() {
				@SuppressWarnings("null")
				@Override
				public Ordering f(final Key a2) {
					final int x = a1.compareTo(a2);
					return x < 0 ? Ordering.LT : x == 0 ? Ordering.EQ : Ordering.GT;
				}
			};
		}
	}));

}
