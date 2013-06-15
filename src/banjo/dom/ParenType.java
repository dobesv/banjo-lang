package banjo.dom;

import org.eclipse.jdt.annotation.Nullable;

public enum ParenType {
	PARENS('(', ')'),
	/** Braces, aka curly brackets, are used to denote object literals */
	BRACES('{', '}'),
	/** Brackets, aka square brackets, are used to denote list literals */
	BRACKETS('[', ']');

	private final char startChar;
	private final char endChar;

	ParenType(char startCh, char endCh) {
		this.startChar = startCh;
		this.endChar = endCh;
	}

	public char getStartChar() {
		return this.startChar;
	}

	public char getEndChar() {
		return this.endChar;
	}

	public static @Nullable ParenType forChar(char startChar) {
		for(final ParenType t : values()) {
			if(startChar == t.startChar)
				return t;
		}
		return null;
	}

	public static @Nullable ParenType forCloseChar(char endChar) {
		for(final ParenType t : values()) {
			if(endChar == t.endChar)
				return t;
		}
		return null;
	}

	public static boolean isOpenParen(int codePoint) {
		for(final ParenType t : values()) {
			if(codePoint == t.startChar)
				return true;
		}
		return false;
	}
	public static boolean isCloseParen(int codePoint) {
		for(final ParenType t : values()) {
			if(codePoint == t.endChar)
				return true;
		}
		return false;
	}

	public String getEmptyPairString() {
		return ""+this.startChar+this.endChar;
	}
}
