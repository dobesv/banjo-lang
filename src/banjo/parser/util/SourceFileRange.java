package banjo.parser.util;

import static java.util.Objects.requireNonNull;
import fj.Ord;
import fj.data.List;

public class SourceFileRange {
	public static final List<SourceFileRange> EMPTY_LIST = List.nil();
	public static final Ord<SourceFileRange> ORD = Ord.ord(
			(a) -> (b) -> a.sourceFile.equals(b.sourceFile) ? FileRange.ORD.compare(a.fileRange, b.fileRange) : Ord.stringOrd.compare(a.sourceFile, b.sourceFile)
	);
	public static final Ord<List<SourceFileRange>> LIST_ORD = Ord.listOrd(SourceFileRange.ORD);

	final String sourceFile;
	final FileRange fileRange;
	public SourceFileRange(String sourceFile, FileRange fileRange) {
		super();
		this.sourceFile = requireNonNull(sourceFile);
		this.fileRange = requireNonNull(fileRange);
	}
	public String getSourceFile() {
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
}
