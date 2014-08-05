package banjo.dom.token;

import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.BadExpr;
import banjo.dom.Expr;
import banjo.dom.core.AnonymousKey;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.CoreExprAlgebra;
import banjo.dom.core.CoreExprVisitor;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExpr;
import banjo.dom.source.SourceExprVisitor;
import banjo.parser.util.SourceFileRange;
import fj.F;
import fj.Ord;
import fj.Ordering;
import fj.data.List;


public interface Key extends SourceExpr, CoreExpr {
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

	public static final Key ANONYMOUS = new AnonymousKey();

	/**
	 * Get a list of name parts for this key.
	 *
	 * For Key.ANONYMOUS, the list is empty.
	 *
	 * For a MixfixFunctionIdentifier the list has two or more names.
	 *
	 * For all others, the key has just one part.
	 */
	public List<String> getParts();
}
