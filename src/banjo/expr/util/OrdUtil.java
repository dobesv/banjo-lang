package banjo.expr.util;

import fj.Ord;
import fj.Ordering;

public class OrdUtil {
	  /**
	   * Given two Ord instances that compare the given type of object, chain them together so
	   * that the result of the first is returned if it is not EQ, otherwise the result
	   * of the second is returned.  This can be used to reuse an Ord between subclasses
	   * or implementations of an interface.
	   *
	   * @return An Ord instance combining the two provided Ord instances
	   */
	  public static <A> Ord<A> chain(Ord<? super A> ord1, Ord<? super A> ord2) {
		  return Ord.ord(a -> a2 -> {
			  Ordering cmp = ord1.compare(a,  a2);
			  if(cmp != Ordering.EQ)
				  return cmp;
			  return ord2.compare(a, a2);
		  });
	  }
}
