package banjo.parser.util;

import org.eclipse.jdt.annotation.Nullable;

public class SourceFileRange implements Comparable<SourceFileRange> {
	final String sourceFile;
	final FileRange fileRange;
	public SourceFileRange(String sourceFile, FileRange fileRange) {
		super();
		this.sourceFile = sourceFile;
		this.fileRange = fileRange;
	}
	public String getSourceFile() {
		return this.sourceFile;
	}
	public FileRange getFileRange() {
		return this.fileRange;
	}
	@Override
	public int hashCode() {
		return this.fileRange.hashCode() ^ this.sourceFile.hashCode();
	}
	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof SourceFileRange))
			return false;
		final SourceFileRange other = (SourceFileRange) obj;
		if (!this.fileRange.equals(other.fileRange))
			return false;
		if (!this.sourceFile.equals(other.sourceFile))
			return false;
		return true;
	}

	@Override
	public int compareTo(@Nullable SourceFileRange o) {
		if(o == null) return -1;
		int cmp = this.sourceFile.compareTo(o.sourceFile);
		if(cmp == 0) cmp = this.fileRange.compareTo(o.fileRange);
		return cmp;
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
