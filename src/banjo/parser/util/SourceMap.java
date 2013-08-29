package banjo.parser.util;

import static banjo.parser.util.Check.nonNull;

import java.util.Iterator;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Expr;
import fj.F;
import fj.Ord;
import fj.Ordering;
import fj.P2;
import fj.data.Set;
import fj.data.TreeMap;


/**
 *
 *
 */
public class SourceMap<T extends Expr> implements Iterable<P2<T,Set<OffsetLength>>> {
	final fj.data.TreeMap<T, fj.data.Set<OffsetLength>> map;
	@SuppressWarnings("null")
	static final fj.data.Set<OffsetLength> EMPTY_FILERANGE_SET = fj.data.Set.empty(Ord.<OffsetLength>comparableOrd());

	private SourceMap(TreeMap<T, Set<OffsetLength>> map) {
		super();
		this.map = map;
	}

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


	/**
	 * Create a new empty source map
	 */
	public SourceMap() {
		this(nonNull(TreeMap.<T, fj.data.Set<OffsetLength>>empty(SourceMap.<T>exprOrd())));
	}

	/**
	 * Return a new SourceMap with the given node mapped to the given file range.
	 */
	@SuppressWarnings("null")
	public SourceMap<T> insert(T node, OffsetLength range) {
		return new SourceMap<T>(this.map.set(node, get(node).insert(range)));
	}

	@SuppressWarnings("null")
	public SourceMap<T> insert(T node, fj.data.Set<OffsetLength> ranges) {
		return new SourceMap<T>(this.map.set(node, get(node).union(ranges)));
	}

	@SuppressWarnings("null")
	public SourceMap<T> union(SourceMap<T> otherMap) {
		SourceMap<T> result = otherMap;
		for(final P2<T, fj.data.Set<OffsetLength>> pair : this.map) {
			result = result.insert(pair._1(), pair._2());
		}
		return result;
	}
	/**
	 * Get the list of ranges the given node has been seen at.  May be an empty set.  Never returns null.
	 */
	@SuppressWarnings("null")
	public Set<OffsetLength> get(T node) {
		return this.map.get(node).orSome(EMPTY_FILERANGE_SET);
	}

	public Iterable<T> nodes() {
		return nonNull(this.map.keys());
	}

	@Override
	public @Nullable Iterator<P2<T,Set<OffsetLength>>> iterator() {
		return this.map.iterator();
	}
}
