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

}
