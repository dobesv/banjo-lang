package banjo.expr.util;

import fj.Ord;
import fj.Ordering;

public class OrdUtil {
	/**
	 * Given two Ord instances that compare the given type of object, chain them
	 * together so that the result of the first is returned if it is not EQ,
	 * otherwise the result of the second is returned. This can be used to reuse
	 * an Ord between subclasses or implementations of an interface.
	 *
	 * @return An Ord instance combining the two provided Ord instances
	 */
	@SafeVarargs
	public static <A> Ord<A> chain(Ord<? super A>... ords) {
		return Ord.ord(a -> a2 -> {
			for (Ord<? super A> ord : ords) {
				Ordering cmp = ord.compare(a, a2);
				if (cmp != Ordering.EQ)
					return cmp;
			}
			return Ordering.EQ;
		});
	}
}
