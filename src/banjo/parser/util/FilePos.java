package banjo.parser.util;

import org.eclipse.jdt.annotation.Nullable;

public final class FilePos {
	/** Absolute file position in characters; this is the count of characters coming BEFORE this position */
	public final int offset;
	/** Line number in the file.  This is the count of line feeds coming before this line, plus 1 for this line (the first line is 1) */
	public final int line;
	/** Column number in the line.  This is the count of characters since the last line feed or the start of the file, plus one (the first column is 1) */
	public final int column;

	public int getOffset() {
		return offset;
	}
	public int getLine() {
		return line;
	}
	public int getColumn() {
		return column;
	}
	public FilePos(int charsRead, int line, int col) {
		super();
		this.offset = charsRead;
		this.line = line;
		this.column = col;
	}

	/**
	 * Create a FilePos for the start of the file.
	 */
	public static FilePos START = new FilePos(0,1,1);
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + offset;
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
		FilePos other = (FilePos) obj;
		if (offset != other.offset)
			return false;
		return true;
	}
	public boolean before(FilePos successor) {
		return offset < successor.offset;
	}
	public boolean after(FilePos predecessor) {
		return offset > predecessor.offset;
	}
	
	
	@Override
	public String toString() {
		return "line "+line+" col "+column; //+ " offset "+offset;
	}
	public String toString(FilePos start) {
		if(start.line == this.line)
			return String.valueOf(column-1); //+" offset "+offset;
		else if(this.column == 1) 
			return "line "+line;
		else
			return "line "+line+" col "+(column-1); //+ " offset "+offset;
	}
	public void toString(StringBuffer sb) {
		sb.append("line ").append(line).append(" col ").append(column);//.append(" offset ").append(offset);
	}
	public void toString(FilePos start, StringBuffer sb) {
		if(start.line != this.line) {
			sb.append("line ").append(line);
		}		
		sb.append(" col ").append(column);//.append(" offset ").append(offset);
	}
	
	/**
	 * Calculate the file position for the first column of the same line.
	 */
	public FilePos lineStart() {
		if(column == 1) return this;
		return new FilePos(offset-column+1, line, 1);
	}
	
}
