package banjo.parser;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.LinkedList;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.AtomVisitor;
import banjo.dom.Comment;
import banjo.dom.Ellipsis;
import banjo.dom.Identifier;
import banjo.dom.NumberLiteral;
import banjo.dom.OperatorRef;
import banjo.dom.StringLiteral;
import banjo.dom.StringLiteral.BadStringEscapeSequence;
import banjo.dom.TokenVisitor;
import banjo.dom.Whitespace;
import banjo.parser.errors.BanjoParseException;
import banjo.parser.errors.EmptyBacktick;
import banjo.parser.errors.MissingDigitsAfterDecimalPoint;
import banjo.parser.errors.PrematureEndOfFile;
import banjo.parser.errors.SyntaxError;
import banjo.parser.errors.UnexpectedDecimalPoint;
import banjo.parser.errors.UnexpectedSecondDecimalPoint;
import banjo.parser.util.FilePos;
import banjo.parser.util.FileRange;
import banjo.parser.util.ParserReader;
import banjo.parser.util.ParserReader.Pos;
import fj.data.Option;

public class BanjoScanner {
	private final LinkedList<BanjoParseException> errors = new LinkedList<>();
	boolean eof = false;
	
	public @Nullable <T> T scan(String inStr, TokenVisitor<T> visitor) {
		try {
			return scan(ParserReader.fromString("<string>", inStr), visitor);
		} catch (IOException e) {
			throw new Error(e);
		}
	}
	
	public static boolean isIdentifierStart(int cp) {
		return cp == '_' || cp == '$' || cp == '\\' || Character.isLetter(cp);
	}
	
	protected @Nullable <T> T scanOne(ParserReader in, TokenVisitor<T> visitor) throws IOException {
		for(;;) {
			in.getCurrentPosition(tokenStartPos);
			Option<T> result = Option.<T>none();
			if( (result = whitespace(in, visitor)).isSome() ||
			    (result = comment(in, visitor)).isSome() ||
			    (result = ellipsis(in, visitor)).isSome() ||
				(result = identifier(in, visitor)).isSome() ||
				(result = numberLiteral(in, visitor)).isSome() ||
				(result = operator(in, visitor)).isSome() ||
				(result = stringLiteral(in, visitor)).isSome()) {
				return result.some();
			} else {
				// Invalid character ?
				int badCp = in.read();
				if(badCp == -1) {
					eof = true;
					return visitor.visitEof();
				} else {
					errors.add(new SyntaxError("Invalid character "+badCp, in.getFileRange(tokenStartPos)));
				}
			}
		}
	}
	
	public @Nullable <T> T scan(ParserReader in, TokenVisitor<T> visitor) throws IOException {
		eof = false;
		for(;;) {
			T result = scanOne(in, visitor);
			if(eof)
				return result;
		}
	}
	
	public @Nullable <T> T scan(ParserReader in, TokenVisitor<T> visitor, Collection<BanjoParseException> errors) throws IOException {
		T result = scan(in, visitor);
		errors.addAll(this.errors);
		return result;
	}
	
	public @Nullable <T> T scan(String source, TokenVisitor<T> visitor, Collection<BanjoParseException> errors) throws IOException {
		return scan(ParserReader.fromString("<string>", source), visitor, errors);
	}

	
	static final int NBSP = '\u00A0';
	static final int NNBSP = '\u202F';
	static final int ELLIPSIS = '\u2026';
	
	
	public static boolean isIdentifierPart(int cp) {
		return isIdentifierStart(cp) 
				|| Character.isDigit(cp)
				|| cp == NBSP
				|| cp == NNBSP
				;
	}
	
	StringBuffer buf = new StringBuffer(100);
	/**
	 * Match an identifier at the current read position.  Returns the identifier string if there is a match,
	 * null otherwise.  On failure the input position will reset to the same position as before 
	 * the call.
	 *  
	 * @return A string if successful; null otherwise
	 * @throws IOException
	 */
	public @Nullable String matchID(ParserReader in) throws IOException {
		final int first = in.read();
		if(!isIdentifierStart(first)) {
			in.unread();
			return null;
		}
		boolean escape = first == '\\';
		buf.setLength(0); // Reset buffer
		if(!escape)
			buf.appendCodePoint(first);
		for(;;) {
			int cp = in.read();
			if(!escape && cp == '\\') {
				escape = true;
			} else {
				if(cp == -1 || !(escape || isIdentifierPart(cp))) {
					in.unread();
					return buf.toString();
				}
				buf.appendCodePoint(cp);
				escape = false;
			}
		}
	}
	
	public static final boolean isOperatorChar(int codePoint) {
		switch(codePoint) {
		case '"': 
		case '\'':
		case '_':
		case '\\':
			return false;
		case '-':
		case '^':
			return true;
		default:
		}
		switch(Character.getType(codePoint)) {
		case Character.MATH_SYMBOL:
		case Character.OTHER_SYMBOL:
		case Character.CONNECTOR_PUNCTUATION:
		case Character.OTHER_PUNCTUATION:
			return true;
		default:
			return false;
		}
	}
	@Nullable
	public String matchOperator(ParserReader in) throws IOException {
		int first = in.read();
		switch(first) {
			case ',': return ",";
			case ';': return ";";
			case '(': return "(";
			case ')': return ")";
			case '[': return "[";
			case ']': return "]";
			case '{': return "{";
			case '}': return "}";
		}
		if(!isOperatorChar(first)) {
			in.unread();
			return null;
		}
			
		buf.setLength(0);
		buf.appendCodePoint(first);
		for(;;) {
			int cp = in.read();
			if(!isOperatorChar(cp)) {
				in.unread();
				break;
			}
			buf.appendCodePoint(cp);
		}
		return buf.toString();
	}
	public static boolean isWhitespaceChar(int codePoint) {
		return Character.isWhitespace(codePoint);
	}
	public <T> Option<T> comment(ParserReader in, TokenVisitor<T> visitor) throws IOException {
		for(;;) {
			int cp = in.read();
			if(cp == '/') {
				cp = in.read();
				if(cp == '/') {
					skipToEndOfLine(in);
				} else if(cp == '*') {
					skipToEndOfMultilineComment(in);
				} else {
					// Not a comment, and thus also not whitespace any more
					in.seek(tokenStartPos);
					return Option.none();
				}
				
				return Option.some(visitor.visitComment(new Comment(in.getFileRange(tokenStartPos), in.readStringFrom(tokenStartPos))));
			} else {
				in.unread(); // Push back the non-whitespace character we found
				return Option.none();
			}
			// continue
		}
	}

	public <T> Option<T> whitespace(ParserReader in, TokenVisitor<T> visitor) throws IOException {
		boolean foundWhitespace = false;
		for(int cp = in.read(); isWhitespaceChar(cp); cp = in.read()) {
			foundWhitespace = true;
		}
		in.unread(); // Push back the non-whitespace character we found
		if(!foundWhitespace)
			return Option.none(); // No whitespace found
		return Option.some(visitor.visitWhitespace(new Whitespace(in.getFileRange(tokenStartPos))));
	}
	
	/**
	 * Assuming we just passed a '/' and a '*', read until we find the matching 
	 * '*' and '/' sequence.
	 */
	public void skipToEndOfMultilineComment(ParserReader in) throws IOException {
		for(;;) {
			int cp = in.read();
			if(cp == '*' || cp == -1) {
				cp = in.read();
				if(cp == -1 || cp == '/')
					break;
			}
		}
	}

	/**
	 * Skip characters until we reach a new line
	 */
	public void skipToEndOfLine(ParserReader in) throws IOException {
		int currentLine = in.getCurrentLineNumber();
		while(in.read() != -1 && in.getCurrentLineNumber() == currentLine) {
			// continue
		}
	}

	private final Pos tokenStartPos = new Pos();
	
	/**
	 * Check whether the input stream is positioned before a string literal and, if so, parse it and return a StringLiteral node.  If not,
	 * leave the input in the same position as given and return null.
	 * 
	 * The parser supports the following escape sequences (prefixed with a backslash ('\') character:
	 * 
	 * <table>
	 * <thead><tr><td>Escape</td><td>Unicode Ordinal</td><td>Comment</td></thead>
	 * <tbody>
	 * <tr><td>\</td><td>92</td><td>Literal Backslash</td></tr>
	 * <tr><td>a</td><td>7</td><td>Bell</td></tr>
	 * <tr><td>e</td><td>27</td><td>Escape</td></tr> 
	 * <tr><td>f</td><td>12</td><td>Form Feed</td></tr> 
	 * <tr><td>n</td><td>13</td><td>Carriage Return</td></tr>
	 * <tr><td>r</td><td>10</td><td>Line Feed</td></tr>
	 * <tr><td>t</td><td>9</td><td>Horizontal Tab</td></tr>
	 * <tr><td>ooo</td><td></td><td>One to three octal digits will be interpreted as an octal number in the range 0 to 0777 (decimal 1024)</td></tr>
	 * <tr><td>xXX{2}</td><td></td><td>Two hex digits give a ASCII character code in the range 0-127</td></tr>
	 * <tr><td>uXXXX</td><td></td><td>Four hex digits give a unicode character in the range 0-65535</td></tr>
	 * </tbody>
	 * </table>
	 */
	
	public <T> Option<T> stringLiteral(ParserReader in, AtomVisitor<T> visitor) throws IOException {
		in.getCurrentPosition(tokenStartPos);
		
		int cp = in.read();
		if(cp == '`') {
			return backtick(in, visitor);
		}
		if(cp != '"' && cp != '\'') {
			in.seek(tokenStartPos);
			return Option.none();
		}
		int quoteType = cp;
			
		int leftColumn = in.getCurrentColumnNumber();
	    buf.setLength(0);
	    while((cp = in.read()) != -1) {
	    	if(cp == quoteType)
	    		break; // End of string
	    	// Ignore whitespace with column <= the left column
	    	if(cp == ' ' && in.getCurrentColumnNumber() <= leftColumn) {
	    		continue;
	    	}
	        if(cp != '\\') {
	            buf.appendCodePoint(cp);
	            continue;
	        }
	        FilePos afterBackslash = in.getFilePos();
	        cp = in.read();
	        if(cp == -1) {
	            errors.add(new BadStringEscapeSequence("Backslash at end of string.", in.getFileRange(tokenStartPos))); // Shouldn't normally be possible
	            break;
	        }
	        switch (cp) {
	        	case '\\': buf.append('\\'); break;
	            case 'a':  buf.append('\007'); break;
	            case 'e':  buf.append('\033'); break;
	            case 'r':  buf.append('\r'); break;
	            case 'n':  buf.append('\n'); break;
	            case 'f':  buf.append('\f'); break;
	            case 't':  buf.append('\t'); break;
	            case '\'': case '"': buf.append((char)cp); break;
	            case '0': 
	            case '1':
	            case '2':
	            case '3':
	            case '4':
	            case '5':
	            case '6':
	            case '7': {
	            	octalEscape(in, cp, buf);
	                break;
	            }
	
	            case 'x':  {
	            	hexEscape(in, buf, 2);
	                break;
	            }
	
	            case 'u': {
	            	hexEscape(in, buf, 4);
	                break;
	            }
	
	            default:  {
	                errors.add(new BadStringEscapeSequence("Unknown escape sequence "+codePointToString(cp), in.getFileRange(afterBackslash)));
	                break;
	            }
	        }
	    }
	    if(cp == -1) errors.add(new PrematureEndOfFile("End of file in string literal", in.getFileRange(tokenStartPos)));
		return Option.some(visitor.visitStringLiteral(new StringLiteral(in.getFileRange(tokenStartPos), buf.toString())));
	}

	private <T> Option<T> backtick(ParserReader in, AtomVisitor<T> visitor) throws IOException {
		String str = matchOperator(in);
		if(str == null) str = matchID(in);
		if(str == null) {
			errors.add(new EmptyBacktick(in.getFileRange(tokenStartPos)));
			str = "";
		}
		final FileRange range = in.getFileRange(tokenStartPos);
		return Option.some(visitor.visitStringLiteral(new StringLiteral(range, str)));
	}

	private void hexEscape(ParserReader in, StringBuffer buf, final int digitCount) throws IOException {
		int result = 0;
		FilePos afterDigits = in.getFilePos();
		for(int digits = 0; digits < digitCount; digits++) {
			int digitValue = Character.digit(in.read(), 16);
			if(digitValue == -1) {
		    	errors.add(new BadStringEscapeSequence("Invalid hex digit in \\x escape", in.getFileRange(afterDigits)));
		    	in.seek(afterDigits);
		    	return;
			} else {
				result = (result << 4) | digitValue;
				afterDigits = in.getFilePos();
			}
		}
		buf.appendCodePoint(result);
	}

	private void octalEscape(ParserReader in, int cp, StringBuffer buf) throws IOException {
		int result = Character.digit(cp, 8);
		FilePos afterDigits = in.getFilePos();
		for(int digits = 1; digits < 3; digits++) {
			int digitValue = Character.digit(in.read(), 8);
			if(digitValue == -1) {
				in.seek(afterDigits);
				return;
			} else {
				result = (result << 3) + digitValue;
				afterDigits = in.getFilePos();
			}
		}
		buf.appendCodePoint(result);
	}

	public <T> Option<T> numberLiteral(ParserReader in, AtomVisitor<T> visitor) throws IOException {
		in.getCurrentPosition(tokenStartPos);
		int cp = in.read();
		boolean negative = false;
		boolean isNumber = false;
		if(cp == '-') {
			// Leading '-' for a negative number of any kind
			negative = true;
			cp = in.read();
		} else if(cp == '+') {
			// Leading + is OK (but useless)
			cp = in.read();
		}
		// Check for a different base
		int radix = 10;
		String formatName = "decimal";
		int maxLongDigits = 18; // the most decimal digits that can SAFELY be kept in a 62-bit integer (we leave one for the sign bit)
		if(cp == '0') {
			isNumber = true;
			cp = in.read();
			switch(cp) {
			case 'x': case 'X': radix = 16; maxLongDigits=15; formatName="hexadecimal"; cp = in.read(); break;
			case 'b': case 'B': radix = 2; maxLongDigits=62; formatName="binary"; cp = in.read(); break;
			case 'o': case 'O': radix = 8; maxLongDigits=20; formatName="octal"; cp = in.read(); break;
			}
		}
		Pos afterDigits = new Pos();
		int digits = 0;
		int digitsLeftOfDecimalPoint = -1;
		int exp = 0;
		long intValLong = 0;
		BigInteger intValBig=null;
		BigInteger radixBig=null;
		for(;;) {
			if(cp == '.') {
				if(radix != 10) {
					errors.add(new UnexpectedDecimalPoint("Decimal point found in "+formatName+" number", in.getFileRange(afterDigits)));
					radix = 10;
				}
				if(digitsLeftOfDecimalPoint != -1) {
					if(!isNumber) {
						in.seek(tokenStartPos);
						return Option.none();
					} else {
						errors.add(new UnexpectedSecondDecimalPoint("Second decimal point in number", in.getFileRange(afterDigits)));
						in.seek(afterDigits);
						break;
					}
				} else {
					digitsLeftOfDecimalPoint = digits;
					cp = in.read();
				}
			} else if(digits > 0 && cp == '_') {
				// Allow underscore to "break up" long numbers, like in Java
				cp = in.read();
			} else if(radix==10 && (cp == 'e' || cp == 'E')) {
				// Can't start a number with an exponent
				if(!isNumber) {
					in.seek(tokenStartPos);
					return Option.none();
				}
				cp = in.read();
				boolean negexp = cp == '-';
				if(negexp || cp == '+')
					cp = in.read();
				for(int expDigits = 0; expDigits < 10; expDigits++) {
					if(cp == '_' && expDigits > 0) {
						cp = in.read();
					}
					int digitValue = Character.digit(cp, 10); // Always base 10
					if(digitValue == -1) {
						in.seek(afterDigits);
						break;
					}
					exp = exp * 10 + digitValue;
					in.getCurrentPosition(afterDigits);
					cp = in.read();
				}
				if(negexp) exp = -exp;
				break;
			} else {
				int digitValue = Character.digit(cp, radix);
				if(digitValue == -1) {
					if(!isNumber) {
						in.seek(tokenStartPos);
						return Option.none();
					} else {
						// Number ends when we find any non-number character
						in.unread();
						break;
					}
				} else {
					if(intValBig != null) {
						intValBig = intValBig.multiply(radixBig).add(BigInteger.valueOf(digitValue));
					} else if(digits == maxLongDigits) {
						radixBig = BigInteger.valueOf(radix);
						intValBig = BigInteger.valueOf(intValLong).multiply(radixBig).add(BigInteger.valueOf(digitValue));
					} else {
						intValLong = (intValLong * radix) + digitValue;
					}
					digits ++;
					isNumber = true;
					in.getCurrentPosition(afterDigits);
					cp = in.read();
				}
			}
		}
		final boolean isInteger = digitsLeftOfDecimalPoint == -1;		
		final int digitsRightOfDecimalPoint = digits - digitsLeftOfDecimalPoint;
		if(!isInteger && digitsRightOfDecimalPoint == 0) {
			if(!isNumber) {
				in.seek(tokenStartPos);
				return Option.none();
			} else {
				errors.add(new MissingDigitsAfterDecimalPoint("Missing digits after decimal point", in.getFileRange(tokenStartPos)));
			}
		}
		final int scale = (isInteger ? 0 : digitsRightOfDecimalPoint) - exp; // TODO Check for overflow on the scale; we might have to restrict scale to 32 bits
		Number number;
		if(intValBig == null) {
			if(negative) {
				intValLong = -intValLong;
			}
			if(isInteger) {
				number = Long.valueOf(intValLong);
			} else {
				number = BigDecimal.valueOf(intValLong, scale);
			}
		} else {
			if(negative) {
				intValBig = intValBig.negate();
			}
			if(isInteger) {
				number = intValBig;
			} else {
				number = new BigDecimal(intValBig, scale);
			}
		}
		String text = in.readStringFrom(tokenStartPos);
		final NumberLiteral result = new NumberLiteral(in.getFileRange(tokenStartPos), text, number);
		return Option.some(visitor.visitNumberLiteral(result));
	}

	/**
	 * Parse a simple identifier expression.  If the current parse position has an
	 * identifier, this consumes it and returns it.  Otherwise, this resets the parse
	 * position and returns null.
	 * 
	 * @return An IdRef if the parse is successful; null otherwise.
	 */
	private <T> Option<T> identifier(ParserReader in, AtomVisitor<T> visitor) throws IOException {
		in.getCurrentPosition(tokenStartPos);
		String identifier = matchID(in);
		if(identifier == null)
			return Option.none();
		FileRange identRange = in.getFileRange(tokenStartPos);
		return Option.some(visitor.visitIdentifier(new Identifier(identRange, identifier)));
	}

	/**
	 * Parse an operator.  If the current parse position has an operator,
	 * this consumes it and returns it.  Otherwise, this resets the parse
	 * position and returns null.
	 * 
	 * @return An OperatorRef if the parse is successful; O otherwise
	 */
	private <T> Option<T> operator(ParserReader in, TokenVisitor<T> visitor) throws IOException {
		in.getCurrentPosition(tokenStartPos);
		String operator = matchOperator(in);
		if(operator == null)
			return Option.none();
		FileRange opRange = in.getFileRange(tokenStartPos);
		return Option.some(visitor.visitOperator(new OperatorRef(opRange, operator)));
	}
	
	/**
	 * Check for an Ellipsis at the current parse position.
	 */
	public <T> Option<T> ellipsis(ParserReader in, TokenVisitor<T> visitor) throws IOException {
		if(in.checkNextChar('.')) {
			in.getPreviousPosition(tokenStartPos);
			if(in.read() == '.' && in.read() == '.') {
				return Option.some(visitor.visitEllipsis(new Ellipsis(in.getFileRange(tokenStartPos))));
			}
			in.seek(tokenStartPos);
		} else if(in.checkNextChar(ELLIPSIS)) {
			return Option.some(visitor.visitEllipsis(new Ellipsis(in.getFileRange(in.getPreviousPosition(tokenStartPos)))));
		}
		return Option.none();
	}

	public static String codePointToString(int cp) {
		return new String(Character.toChars(cp));
	}

	public LinkedList<BanjoParseException> getErrors() {
		return errors;
	}


}
