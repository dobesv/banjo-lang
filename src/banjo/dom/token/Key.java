package banjo.dom.token;

import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.core.CoreExpr;
import fj.F;
import fj.Ord;
import fj.Ordering;


public interface Key extends Atom, CoreExpr {
	public String getKeyString();

	public static final Ord<Key> ORD = nonNull(Ord.ord(new F<Key, F<Key, Ordering>>() {
		@Override
		public F<Key, Ordering> f(final @Nullable Key a1) {
			return new F<Key, Ordering>() {
				@SuppressWarnings("null")
				@Override
				public Ordering f(final @Nullable Key a2) {
					final int x = a1 == a2 ? 0 : a1 == null ? 1 : a2 == null ? -1 : a1.compareTo(a2);
					return x < 0 ? Ordering.LT : x == 0 ? Ordering.EQ : Ordering.GT;
				}
			};
		}
	}));

}
