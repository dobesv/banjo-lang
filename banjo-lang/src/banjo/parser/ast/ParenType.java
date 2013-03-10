package banjo.parser.ast;

public enum ParenType {
	PARENS('(', ')'),
	/** Braces, aka curly brackets, are used to denote object literals */
	BRACES('{', '}'),
	/** Brackets, aka square brackets, are used to denote list literals */
	BRACKETS('[', ']');
	
	private int startChar;
	private int endChar;

	ParenType(int startCh, int endCh) {
		this.startChar = startCh;
		this.endChar = endCh;
	}

	public int getStartChar() {
		return startChar;
	}

	public void setStartChar(int startChar) {
		this.startChar = startChar;
	}

	public int getEndChar() {
		return endChar;
	}

	public void setEndChar(int endChar) {
		this.endChar = endChar;
	}
	
	public static ParenType forCodePoint(int startChar) {
		for(ParenType t : values()) {
			if(startChar == t.startChar)
				return t;
		}
		return null;
	}
}
