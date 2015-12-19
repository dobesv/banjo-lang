package banjo.expr.util;

import static java.util.Objects.requireNonNull;

import java.nio.file.Path;
import java.nio.file.Paths;

import fj.Ord;
import fj.data.Set;

public class SourceFileRange {
	public static final Ord<SourceFileRange> ORD = OrdUtil.chain(
			Ord.<Path>comparableOrd().contramap(sfr -> sfr.sourceFile),
			FileRange.ORD.contramap(sfr -> sfr.fileRange)
	);
	public static final Set<SourceFileRange> EMPTY_SET = Set.empty(ORD);
	public static final Ord<Set<SourceFileRange>> SET_ORD = Ord.setOrd(SourceFileRange.ORD);

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

    public static Set<SourceFileRange> currentJavaThreadLoc() {
	    StackTraceElement[] trace = Thread.currentThread().getStackTrace();
        for(int i = 2; i < trace.length; i++) {
            StackTraceElement frame = trace[i];
            if(frame.getFileName() != null && frame.getLineNumber() > 0) {
                FilePos pos = new FilePos(0, frame.getLineNumber(), 0);
                return Set.single(ORD, new SourceFileRange(
                    Paths.get(frame.getFileName()),
                    new FileRange(pos, pos)));
            }
        }
        return EMPTY_SET;
	}

    /**
     * Add this to the given set, but try to merge with any adjacent range(s)
     */
    public Set<SourceFileRange> insertInto(Set<SourceFileRange> ranges) {
        for(SourceFileRange r : ranges) {
            if(r.getSourceFile().equals(sourceFile)) {
                if(r.fileRange.getEnd().equals(this.fileRange.getStart())) {
                    return ranges.delete(r).insert(new SourceFileRange(
                        this.sourceFile, new FileRange(r.fileRange.getStart(), this.fileRange.getEnd())));
                } else if(r.getFileRange().getStart().equals(this.fileRange.getEnd())) {
                    return ranges.delete(r).insert(new SourceFileRange(
                        this.sourceFile, new FileRange(this.fileRange.getStart(), r.fileRange.getEnd())));
                }
            }
        }
        return ranges.insert(this);
    }

    /**
     * Merge adjacent ranges in the given set
     */
    public static Set<SourceFileRange> compactSet(Set<SourceFileRange> a) {
        return a.toStream().foldLeft((ranges, r) -> r.insertInto(ranges), Set.empty(ORD));
    }

    /**
     * Add new ranges into the first set, merging any new ranges from b with
     * existing ranges in a.
     * 
     * @param a
     *            Ranges we already have
     * @param b
     *            New ranges we want to extend the set with
     * @return A new set that covers all the ranges from a & b
     */
    public static Set<SourceFileRange> union(Set<SourceFileRange> a, Set<SourceFileRange> b) {
        return b.toStream().foldLeft((ranges, r) -> r.insertInto(ranges), a);
    }
}
