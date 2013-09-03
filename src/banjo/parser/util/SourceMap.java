package banjo.parser.util;

import static banjo.parser.util.Check.nonNull;

import java.util.Iterator;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Expr;
import fj.Ord;
import fj.P2;
import fj.data.Set;
import fj.data.TreeMap;


/**
 *
 *
 */
public class SourceMap<T extends Expr> implements Iterable<P2<T,Set<FileRange>>> {
	final fj.data.TreeMap<T, fj.data.Set<FileRange>> map;
	@SuppressWarnings("null")
	public static final fj.data.Set<FileRange> EMPTY_RANGE_SET = fj.data.Set.empty(Ord.<FileRange>comparableOrd());

	private SourceMap(TreeMap<T, Set<FileRange>> map) {
		super();
		this.map = map;
	}

	/**
	 * Create a new empty source map
	 */
	public SourceMap() {
		this(nonNull(TreeMap.<T, fj.data.Set<FileRange>>empty(ExprOrd.<T>exprOrd())));
	}

	/**
	 * Return a new SourceMap with the given node mapped to the given file range.
	 */
	@SuppressWarnings("null")
	public SourceMap<T> insert(T node, FileRange range) {
		return new SourceMap<T>(this.map.set(node, get(node).insert(range)));
	}

	@SuppressWarnings("null")
	public SourceMap<T> insert(T node, fj.data.Set<FileRange> ranges) {
		return new SourceMap<T>(this.map.set(node, get(node).union(ranges)));
	}

	@SuppressWarnings("null")
	public SourceMap<T> union(SourceMap<T> otherMap) {
		SourceMap<T> result = otherMap;
		for(final P2<T, fj.data.Set<FileRange>> pair : this.map) {
			result = result.insert(pair._1(), pair._2());
		}
		return result;
	}
	/**
	 * Get the list of ranges the given node has been seen at.  May be an empty set.  Never returns null.
	 */
	@SuppressWarnings("null")
	public Set<FileRange> get(T node) {
		return this.map.get(node).orSome(EMPTY_RANGE_SET);
	}

	public Iterable<T> nodes() {
		return nonNull(this.map.keys());
	}

	@Override
	public @Nullable Iterator<P2<T,Set<FileRange>>> iterator() {
		return this.map.iterator();
	}

	/**
	 * Get the first file range within the given parent range for the given node.
	 * 
	 * If there's no range found that matches, returns the parent range (bounds).
	 */
	public FileRange getWithin(FileRange bounds, T node) {
		final Set<FileRange> all = get(node);
		FileRange result = bounds;
		for(final FileRange candidate : all) {
			// Candidate must be a subrange of the bounds
			if(!candidate.isSubrange(bounds))
				continue;
			if(result == bounds || candidate.getStart().before(result.getStart())) {
				result = candidate;
			}
		}
		return result;
	}

	/**
	 * Get the first file range for the given node.
	 * 
	 * If there's no range found that matches, returns null.
	 */
	@Nullable
	public FileRange getFirst(T node) {
		final Set<FileRange> all = get(node);
		@Nullable
		FileRange result = null;
		for(final FileRange candidate : all) {
			if(result == null || candidate.getStart().before(result.getStart())) {
				result = candidate;
			}
		}
		return result;
	}
}
