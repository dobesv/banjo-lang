package banjo.parser.util;

import fj.Ord;
import fj.Ordering;


public final class FilePos {
	public static final Ord<FilePos> ORD = Ord.ord((a) -> (b) -> Ordering.fromInt(b.offset-a.offset));
	/** Absolute file position in characters; this is the count of characters coming BEFORE this position */
	public final int offset;
	/** Line number in the file.  This is the count of line feeds coming before this line, plus 1 for this line (the first line is 1) */
	public final int line;
	/** Column number in the line.  This is the count of characters since the last line feed or the start of the file, plus one (the first column is 1) */
	public final int column;

	public int getOffset() {
		return this.offset;
	}
	public int getLine() {
		return this.line;
	}
	public int getColumn() {
		return this.column;
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
		result = prime * result + this.offset;
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
		final FilePos other = (FilePos) obj;
		if (this.offset != other.offset)
			return false;
		return true;
	}
	public boolean before(FilePos successor) {
		return this.offset < successor.offset;
	}
	public boolean after(FilePos predecessor) {
		return this.offset > predecessor.offset;
	}


	@Override
	public String toString() {
		return "line "+this.line+" col "+this.column; //+ " offset "+offset;
	}
	public String toString(FilePos start) {
		if(start.line == this.line)
			return String.valueOf(this.column-1); //+" offset "+offset;
		else if(this.column == 1)
			return "end of line "+(this.line-1);
		else
			return "line "+this.line+" col "+(this.column-1); //+ " offset "+offset;
	}
	public void toString(StringBuffer sb) {
		sb.append("line ").append(this.line).append(" col ").append(this.column);//.append(" offset ").append(offset);
	}
	public void toString(FilePos start, StringBuffer sb) {
		if(start.line != this.line) {
			sb.append("line ").append(this.line);
		}
		sb.append(" col ").append(this.column);//.append(" offset ").append(offset);
	}

	/**
	 * Calculate the file position for the first column of the same line.
	 */
	public FilePos lineStart() {
		if(this.column == 1) return this;
		return new FilePos(this.offset-this.column+1, this.line, 1);
	}

	/**
	 * Calculate a the new FilePos we'd get after processing the given
	 * characters starting at this position.
	 */
	public FilePos afterChars(String text) {
		// TODO We're assuming '\n' is the line delimeter, eclipse doesn't do that ... hmmm ....
		int line = this.line;
		int column = this.column;
		for(int i=0; i < text.length(); i++) {
			if(text.charAt(i) == '\n') {
				column = 1;
				line ++;
			} else {
				column ++;
			}
		}
		return new FilePos(this.offset+text.length(), line, column);
	}

}
