package banjo.parser.util;


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
	}
	public FileRange(FileRange head, FileRange tail) {
		this(head.getFilename(), head.getStart(), tail.getEnd());
		if(!head.getFilename().equals(tail.getFilename())) throw new IllegalStateException(); // Don't expect parse trees to span multiple files, do we?
		if(end.before(start)) throw new IllegalStateException("Range end comes before start"); // Don't expect nodes to be out of order in that array
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
	public boolean equals(Object obj) {
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
		if(!getStart().equals(getEnd())) {
			sb.append(" to ");
			sb.append(getEnd().toString(getStart()));
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
	public int getEndLine() {
		return end.line;
	}
	public int getStartColumn() {
		return start.column;
	}

}
