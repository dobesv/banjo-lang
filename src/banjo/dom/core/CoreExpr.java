package banjo.dom.core;

import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Expr;
import fj.F;
import fj.Ord;
import fj.Ordering;

/**
 * Core expressions are the ones that may be emitted by the desugaring process.
 */
public interface CoreExpr extends Expr {
	/**
	 * We are using the visitor pattern, not instanceof, to check the type of an expression.
	 */
	<T> T acceptVisitor(CoreExprVisitor<T> visitor);

	<T> T acceptVisitor(CoreExprAlgebra<T> visitor);

	public static final Ord<CoreExpr> ORD = nonNull(Ord.ord(new F<CoreExpr, F<CoreExpr, Ordering>>() {
		@Override
		public F<CoreExpr, Ordering> f(final @Nullable CoreExpr a1) {
			return new F<CoreExpr, Ordering>() {
				@SuppressWarnings("null")
				@Override
				public Ordering f(final @Nullable CoreExpr a2) {
					if(a1 == null || a2 == null) throw new NullPointerException();
					final int x = a1.compareTo(a2);
					return x < 0 ? Ordering.LT : x == 0 ? Ordering.EQ : Ordering.GT;
				}
			};
		}
	}));

}
