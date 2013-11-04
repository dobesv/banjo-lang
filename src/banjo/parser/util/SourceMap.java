package banjo.parser.util;

import static banjo.parser.util.Check.nonNull;

import java.util.Iterator;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.SourceExpr;
import fj.Ord;
import fj.P2;
import fj.data.Set;
import fj.data.TreeMap;


/**
 *
 *
 */
public class SourceMap implements Iterable<P2<SourceExpr,Set<FileRange>>> {
	final fj.data.TreeMap<SourceExpr, fj.data.Set<FileRange>> map;
	@SuppressWarnings("null")
	public static final fj.data.Set<FileRange> EMPTY_RANGE_SET = fj.data.Set.empty(Ord.<FileRange>comparableOrd());

	private SourceMap(TreeMap<SourceExpr, Set<FileRange>> map) {
		super();
		this.map = map;
	}

	/**
	 * Create a new empty source map
	 */
	public SourceMap() {
		this(nonNull(TreeMap.<SourceExpr, fj.data.Set<FileRange>>empty(ExprOrd.<SourceExpr>exprOrd())));
	}

	public static SourceMap single(SourceExpr node, FileRange range) {
		return new SourceMap().insert(node, range);
	}

	/**
	 * Return a new SourceMap with the given node mapped to the given file range.
	 */
	@SuppressWarnings("null")
	public SourceMap insert(SourceExpr node, FileRange range) {
		return new SourceMap(this.map.set(node, get(node).insert(range)));
	}

	@SuppressWarnings("null")
	public SourceMap insert(SourceExpr node, fj.data.Set<FileRange> ranges) {
		return new SourceMap(this.map.set(node, get(node).union(ranges)));
	}

	@SuppressWarnings("null")
	public SourceMap union(SourceMap otherMap) {
		//final long startTime = System.currentTimeMillis();
		SourceMap result = otherMap;
		for(final P2<SourceExpr, fj.data.Set<FileRange>> pair : this.map) {
			result = result.insert(pair._1(), pair._2());
		}
		//System.out.println("SourceMap.union took "+(System.currentTimeMillis() - startTime)+"ms");
		return result;

	}
	/**
	 * Get the list of ranges the given node has been seen at.  May be an empty set.  Never returns null.
	 */
	@SuppressWarnings("null")
	public Set<FileRange> get(SourceExpr node) {
		return this.map.get(node).orSome(EMPTY_RANGE_SET);
	}

	public Iterable<SourceExpr> nodes() {
		return nonNull(this.map.keys());
	}

	@Override
	public @Nullable Iterator<P2<SourceExpr,Set<FileRange>>> iterator() {
		return this.map.iterator();
	}

	/**
	 * Get the first file range within the given parent range for the given node.
	 * 
	 * If there's no range found that matches, returns the parent range (bounds).
	 */
	public FileRange getFirstWithin(FileRange bounds, SourceExpr node) {
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
	public FileRange getFirst(SourceExpr node) {
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

	/**
	 * True if the given offset is covered by a source expression matching the given node.
	 */
	public boolean coversOffset(int fileOffset, SourceExpr node) {
		for(final FileRange range : get(node)) {
			if(range.containsOffset(fileOffset))
				return true;
		}
		return false;
	}

	static final SourceMap EMPTY = new SourceMap();
	public static SourceMap empty() {
		return EMPTY;
	}

	@SuppressWarnings("null")
	@Override
	public String toString() {
		final StringBuffer sb = new StringBuffer();
		sb.append("SourceMap: {\n");
		for(final P2<SourceExpr, Set<FileRange>> p : this.map) {
			for(final FileRange r : p._2()) {
				sb.append("    ");
				sb.append(p._1().toString());
				sb.append(" @ ");
				sb.append(r.toString());
				sb.append('\n');
			}
		}
		sb.append("}\n");
		return sb.toString();
	}
}
