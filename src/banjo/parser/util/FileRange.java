package banjo.parser.util;

import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.Nullable;


public final class FileRange implements Comparable<FileRange> {
	private final FilePos start;
	private final FilePos end;
	public FileRange(FilePos start, FilePos end) {
		super();
		this.start = start;
		this.end = end;
		if(end.before(start)) {
			throw new IllegalStateException("Range end comes before start"); // Don't expect nodes to be out of order in that array
		}
	}

	/**
	 * Create a new range by extending the first parameter to the end position
	 * of the second parameter.  The second parameter must end after the beginning
	 * of the first parameter;
	 * 
	 * @param head First range, the start of which is used in the new range
	 * @param tail Second range, the end of which is used in the new range
	 */
	public FileRange(FileRange head, FileRange tail) {
		this(head, tail.getEnd());
	}
	/**
	 * Create a new file range by extending an existing one to a new end position.
	 * 
	 * @param baseRange Range to extend
	 * @param newTail New end position
	 */
	public FileRange(FileRange head, FilePos tail) {
		this(head.getStart(), tail);

	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + getEnd().hashCode();
		result = prime * result + getStart().hashCode();
		return result;
	}
	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		final FileRange other = (FileRange) obj;
		if (!this.end.equals(other.end))
			return false;
		if (!getStart().equals(other.getStart()))
			return false;
		return true;
	}
	public boolean startsBefore(FileRange tail) {
		return getStart().before(tail.getStart());
	}

	@Override
	public String toString() {
		final StringBuffer sb = new StringBuffer();
		sb.append(getStart());
		if(this.end.offset > this.start.offset+1) {
			sb.append(" to ");
			sb.append(this.end.toString(this.start));
		}
		return nonNull(sb.toString());
	}
	public FilePos getStart() {
		return this.start;
	}
	public FilePos getEnd() {
		return this.end;
	}
	public int getStartOffset() {
		return getStart().offset;
	}
	public int getEndOffset() {
		return this.end.offset;
	}

	/**
	 * Number of characters between the start and end of the range, including newlines.
	 */
	public int length() {
		return getEndOffset() - getStartOffset();
	}
	public int getStartLine() {
		return this.start.line;
	}
	public int getEndLine() {
		return this.end.line;
	}
	public int getStartColumn() {
		return this.start.column;
	}
	public int getEndColumn() {
		return this.end.column;
	}

	/**
	 * Check whether the given offset falls within this range.
	 * 
	 * @param offset Offset to check
	 * @return true if the given offset is >= getStartOffset() and < getEndOffset().
	 */
	public boolean containsOffset(int offset) {
		return offset >= getStartOffset() && offset < getEndOffset();
	}

	/**
	 * Return a new FileRange with the same start position but the given end position.
	 */
	public FileRange extend(FilePos endPos) {
		return new FileRange(this, endPos);
	}

	/**
	 * Return a new FileRange with the same start position but the end position of the given range.
	 */
	public FileRange extend(FileRange fileRange) {
		return new FileRange(this, fileRange);
	}

	public static FileRange between(FileRange a, FileRange b) {
		return new FileRange(a.getEnd(), b.getStart());
	}

	public FileRange headRange() {
		return new FileRange(this.start, this.start);
	}

	@Override
	public int compareTo(FileRange o) {
		int cmp = Integer.compare(getStartOffset(), o.getStartOffset());
		if(cmp == 0) cmp = Integer.compare(getEndOffset(), o.getEndOffset());
		return cmp;
	}

	public OffsetLength toOffsetLength() {
		return new OffsetLength(getStartOffset(), length());
	}


}
