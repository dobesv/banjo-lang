package banjo.parser.util;

import org.eclipse.jdt.annotation.Nullable;


public final class FileRange {
	private final String filename;
	private final FilePos start;
	private final FilePos end;
	public FileRange(String filename, FilePos start, FilePos end) {
		super();
		if(filename == null) throw new NullPointerException();
		if(start == null) throw new NullPointerException();
		if(end == null) throw new NullPointerException();
		this.filename = filename;
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
		if(!head.getFilename().equals(tail.getFilename())) throw new IllegalStateException(); // Don't expect parse trees to span multiple files, do we?
	}
	/**
	 * Create a new file range by extending an existing one to a new end position.
	 * 
	 * @param baseRange Range to extend
	 * @param newTail New end position
	 */
	public FileRange(FileRange head, FilePos tail) {
		this(head.getFilename(), head.getStart(), tail);
		
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((getEnd() == null) ? 0 : getEnd().hashCode());
		result = prime * result + ((getFilename() == null) ? 0 : getFilename().hashCode());
		result = prime * result + ((getStart() == null) ? 0 : getStart().hashCode());
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
		FileRange other = (FileRange) obj;
		if (!end.equals(other.end))
			return false;
		if (!getFilename().equals(other.getFilename()))
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
		StringBuffer sb = new StringBuffer();
		sb.append(getFilename());
		sb.append(": ");
		sb.append(getStart());
		if(end.offset > start.offset+1) {
			sb.append(" to ");
			sb.append(end.toString(start));
		}
		return sb.toString();
	}
	public FilePos getStart() {
		return start;
	}
	public FilePos getEnd() {
		return end;
	}
	public int getStartOffset() {
		return getStart().offset;
	}
	public int getEndOffset() {
		return end.offset;
	}
	
	/**
	 * Number of characters between the start and end of the range, including newlines.
	 */
	public int length() {
		return getEndOffset() - getStartOffset();
	}
	public String getFilename() {
		return filename;
	}
	public int getStartLine() {
		return start.line;
	}
	public int getEndLine() {
		return end.line;
	}
	public int getStartColumn() {
		return start.column;
	}
	public int getEndColumn() {
		return end.column;
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


}
