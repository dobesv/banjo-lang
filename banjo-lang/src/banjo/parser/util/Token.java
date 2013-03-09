package banjo.parser.util;

public class Token {
	private final String text;
	private final FileRange fileRange;
	
	public Token(FileRange fileRange, String text) {
		if(fileRange == null) throw new NullPointerException();
		if(text == null) throw new NullPointerException();
		if(text.length() != fileRange.length()) throw new IllegalStateException();
		this.fileRange = fileRange;
		this.text = text;
	}
	
	public FileRange getFileRange() {
		return fileRange;
	}
	
	@Override
	public String toString() {
		return getText().replace("\n", "\\n").replace("\r", "\\r"); //+"@("+fileRange+")";
	}

	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result
				+ ((getFileRange() == null) ? 0 : getFileRange().hashCode());
		result = prime * result + ((getText() == null) ? 0 : getText().hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		Token other = (Token) obj;
		if (!getFileRange().equals(other.getFileRange()))
			return false;
		if (!getText().equals(other.getText()))
			return false;
		return true;
	}

	public FilePos getEndPos() {
		return getFileRange().getEnd();
	}

	public String getText() {
		return text;
	}

	public int getStartColumn() {
		return getStartPos().column;
	}

	private FilePos getStartPos() {
		return getFileRange().getStart();
	}
}
