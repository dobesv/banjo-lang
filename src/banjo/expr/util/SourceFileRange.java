package banjo.expr.util;

import static java.util.Objects.requireNonNull;

import java.nio.file.Path;
import java.nio.file.Paths;

import fj.Ord;
import fj.data.List;

public class SourceFileRange {
	public static final List<SourceFileRange> EMPTY_LIST = List.nil();
	public static final Ord<SourceFileRange> ORD = OrdUtil.chain(
			Ord.<Path>comparableOrd().contramap(sfr -> sfr.sourceFile),
			FileRange.ORD.contramap(sfr -> sfr.fileRange)
	);
	public static final Ord<List<SourceFileRange>> LIST_ORD = Ord.listOrd(SourceFileRange.ORD);

	public final Path sourceFile;
	public final FileRange fileRange;
	public SourceFileRange(Path sourceFile, FileRange fileRange) {
		super();
		this.sourceFile = requireNonNull(sourceFile);
		this.fileRange = requireNonNull(fileRange);
	}
	public SourceFileRange(FileRange fileRange) {
		this(Paths.get(""), fileRange);
	}
	
	public Path getSourceFile() {
		return this.sourceFile;
	}
	public FileRange getFileRange() {
		return this.fileRange;
	}

	@Override
	public String toString() {
		return this.sourceFile+":"+this.fileRange;
	}
	public SourceFileRange extend(SourceFileRange other) {
		assert(this.sourceFile.equals(other.sourceFile));
		return new SourceFileRange(this.sourceFile, this.fileRange.extend(other.getFileRange()));
	}
	public SourceFileRange extend(FileRange range) {
		return new SourceFileRange(this.sourceFile, this.fileRange.extend(range));
	}
	public int getStartColumn() {
		return this.fileRange.getStartColumn();
	}
	public int getStartLine() {
		return this.fileRange.getStartLine();
	}
	public int getEndLine() {
		return this.fileRange.getEndLine();
	}

	public static List<SourceFileRange> snoc(List<SourceFileRange> ranges, SourceFileRange range) {
		if(ranges.isNotEmpty()) {
			final SourceFileRange head = ranges.head();
			final List<SourceFileRange> tail = ranges.tail();
			if(tail.isEmpty() && head.sourceFile.equals(range.sourceFile) && head.fileRange.getEndOffset() == range.fileRange.getStartOffset()) {
				return List.single(head.extend(range));
			} else {
				return List.cons(head, snoc(tail, range));
			}
		} else {
			return List.single(range);
		}
	}
	public static List<SourceFileRange> append(List<SourceFileRange> a, List<SourceFileRange> b) {
		if(b.isEmpty()) return a;
		if(a.isEmpty()) return b;
		return b.foldRight((range, ranges) -> snoc(ranges, range), a);
    }
}
