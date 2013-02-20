package banjo.parser;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.text.ParseException;
import java.util.Collection;
import java.util.regex.Pattern;

import banjo.parser.ast.Expr;
import banjo.parser.ast.Expr.Precedence;
import banjo.parser.ast.IdRef;
import banjo.parser.ast.Let;
import banjo.parser.ast.NumberLiteral;
import banjo.parser.ast.StringLiteral;
import banjo.parser.ast.StringLiteral.BadStringEscapeSequence;
import banjo.parser.util.FilePos;
import banjo.parser.util.FileRange;
import banjo.parser.util.ParserReader;
import banjo.parser.util.Token;

/**
 * Change input into an AST.
 */
public class BanjoParser {

	public static Expr parse(ParserReader in, Collection<BanjoParseException> errors) throws IOException, BanjoParseException {
		try {
			return parseAnyExpr(in, Precedence.lowest(), errors);
		} catch(BanjoParseException pe) {
			errors.add(pe);
			return null;
		}
	}
	public static Pattern whitespace = Pattern.compile("([ \r\n]*|//[^\n]*|/\\*.*?\\*/)*", Pattern.DOTALL);
	public static Pattern idPattern = Pattern.compile("[\\p{Alpha}_-][\\p{Alnum}_-]*");
	public static Pattern opPattern = Pattern.compile("((=|\\+|-|/|\\*|&|^|>>|<<|\\^|\\$|\\#|\\@|!|\\|)=?|\\(|\\)|\\{|\\}|\\[|\\])");
	public static Pattern sepPattern = Pattern.compile("[;,]");
	/**
	 * Attempt to parse an ID token; returns null on failure.
	 * 
	 * The parse position will be just after the matched identifier on success, or the
	 * original starting position on failure.
	 * @param in
	 * @return
	 * @throws IOException 
	 */
	public static Token parseID(ParserReader in) throws IOException {
		FilePos startPos = in.getFilePos();
		String ignored = consumeWhitespace(in);
		Token result = in.checkNextToken(idPattern, ignored);
		if(result == null) in.seek(startPos);
		return result;
	}

	public static String consumeWhitespace(ParserReader in) throws IOException {
		return in.consume(whitespace);
	}

	/**
	 * Check whether the given operator follows.  If so, consume it and return true.  If not, return the stream in the
	 * same position and return false;
	 */
	public static boolean checkOperator(ParserReader in, String op) throws IOException {
		FilePos startPos = in.getFilePos();
		String ignored = consumeWhitespace(in);
		Token token = in.checkNextToken(opPattern, ignored);
		boolean result = token != null && token.getText().equals(op);
		if(!result) in.seek(startPos);
		return result;
	}

	/**
	 * Check whether the given separator follows.  If so, consume it and return true.  If not, but the next non-whitespace character
	 * is on a new line from where we started, consume the whitespace and return true.  If that fails, rewind and return false.
	 * 
	 * @param in Source input
	 * @param sep Seperator we are looking for; e.g. ";"
	 * @param indentColumn If a newline is used as a separator, the indentation must be at least this much or a parse exception will be thrown 
	 * @param errors 
	 * @return
	 * @throws IOException
	 * @throws IncorrectIndentation 
	 */
	public static boolean checkSeparatorOrNewline(ParserReader in, String sep, int indentColumn, Collection<BanjoParseException> errors) throws IOException {
		FilePos startPos = in.getFilePos();
		String ignored = consumeWhitespace(in);
		FilePos tokenStartPos = in.getFilePos();
		Token token = in.checkNextToken(sepPattern, ignored);
	    boolean result = token != null && token.getText().equals(sep);
	    if(!result) {
	    	if(tokenStartPos.line > startPos.line) {
	    		in.seek(tokenStartPos);
	    		if(tokenStartPos.col != indentColumn)
	    			errors.add(new IncorrectIndentation(in.getFileRange(in.getFilePos().lineStart()), indentColumn));
	    		return true;
	    	} else {
		    	in.seek(startPos);
		    	return false;
	    	}
	    }
	    return true;
	}
	
	public static class BanjoParseException extends java.text.ParseException {
		private static final long serialVersionUID = 1L;
		private final FileRange range;

		public BanjoParseException(String message, FileRange range) {
			super(message, range.getStartOffset());
			this.range = range;
		}

		public int getStartLine() { return range.getStart().line; }
		public int getStartColumn() { return range.getStart().col; }
		public int getEndLine() { return range.getEnd().line; }
		public int getEndColumn() { return range.getEnd().col; }
	}
	public static class ExpectedSemiColonOrNewline extends BanjoParseException {
		private static final long serialVersionUID = 1L;

		public ExpectedSemiColonOrNewline(FileRange range) {
			super("Expected semicolon or newline", range);
		}
	}
	public static class IncorrectIndentation extends BanjoParseException {
		private static final long serialVersionUID = 1L;

		public IncorrectIndentation(FileRange range, int indentColumn) {
			super("Expected indentation to column "+indentColumn+" but indentation was "+range.getEnd().col+" columns.", range);
		}
	}
	
	public static class PrematureEndOfFile extends BanjoParseException {
		private static final long serialVersionUID = 1L;
	
		public PrematureEndOfFile(String message, FileRange range) {
			super(message, range);
		}
	
	}

	public static class UnexpectedDecimalPoint extends BanjoParseException {
		private static final long serialVersionUID = 1L;
	
		public UnexpectedDecimalPoint(String message, FileRange range) {
			super(message, range);
		}
	
	}
	public static class UnexpectedExponent extends BanjoParseException {
		private static final long serialVersionUID = 1L;

		public UnexpectedExponent(String message, FileRange range) {
			super(message, range);
			// TODO Auto-generated constructor stub
		}
	
	}
	public static class UnexpectedSecondDecimalPoint extends UnexpectedDecimalPoint {
		private static final long serialVersionUID = 1L;

		public UnexpectedSecondDecimalPoint(String message, FileRange range) {
			super(message, range);
		}
	
	}

	/**
	 * Skip whitespace and attempt to parse the next token by matching the given regular
	 * expression.  If the regular expression doesn't match, rewaind to before any skipped
	 * whitespace and return null.  If it does match, consume the token and return it.
	 */
	public static Token checkPattern(ParserReader in, Pattern p) throws IOException {
		FilePos startPos = in.getFilePos();
		String ignored = consumeWhitespace(in);
		Token tok = in.checkNextToken(p, ignored);
		if(tok == null)
			in.seek(startPos);
		return tok;
	}

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
	 *
	 * TODO: Error reporting ...
	 */
	
	public static StringLiteral parseStringLiteral(ParserReader in, Collection<BanjoParseException> errors) throws IOException {
		FilePos startPos = in.getFilePos();
		String ignored = BanjoParser.consumeWhitespace(in);
		
		int cp = in.read();
		if(cp != '"') {
			in.seek(startPos);
			return null;
		}
			
		FilePos tokenStartPos = in.getFilePos();
	    StringBuffer buf = new StringBuffer(in.remaining());
	    while((cp = in.read()) != -1) {
	    	if(cp == '"')
	    		break; // End of string
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
	            	readOctalEscape(in, cp, buf);
	                break;
	            }
	
	            case 'x':  {
	            	readHexEscape(in, errors, buf, 2);
	                break;
	            }
	
	            case 'u': {
	            	readHexEscape(in, errors, buf, 4);
	                break;
	            }
	
	            default:  {
	                errors.add(new BadStringEscapeSequence("Unknown escape sequence "+new String(Character.toChars(cp)), in.getFileRange(afterBackslash)));
	                break;
	            }
	        }
	    }
	    if(cp == -1) errors.add(new BanjoParser.PrematureEndOfFile("End of file in string literal", in.getFileRange(tokenStartPos)));
		return new StringLiteral(in.readTokenFrom(tokenStartPos, ignored), buf.toString());
	}

	private static void readHexEscape(ParserReader in, Collection<BanjoParseException> errors, StringBuffer buf, final int digitCount) throws IOException {
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

	private static void readOctalEscape(ParserReader in, int cp, StringBuffer buf) throws IOException {
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

	public static NumberLiteral parseNumberLiteral(ParserReader in, Collection<BanjoParseException> errors) throws IOException {
		FilePos startPos = in.getFilePos();
		String ignored = BanjoParser.consumeWhitespace(in);
		FilePos tokenStartPos = in.getFilePos();
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
			if(cp == 'x' || cp == 'X') { radix = 16; maxLongDigits=15; formatName="hexadecimal"; }
			else if(cp == 'b' || cp == 'B') { radix = 2; maxLongDigits=62; formatName="binary"; }
			else if(cp == 'o' || cp == 'O') { radix = 8; maxLongDigits=20; formatName="octal"; }
		}
		FilePos afterDigits = in.getFilePos();
		int digits = 0;
		int digitsLeftOfDecimalPoint = -1;
		int exp = 0;
		long intValLong = 0;
		BigInteger intValBig=null;
		BigInteger radixBig=null;
		for(;;) {
			if(cp == '.') {
				if(radix != 10) {
					errors.add(new BanjoParser.UnexpectedDecimalPoint("Decimal point found in "+formatName+" number", in.getFileRange(afterDigits)));
					radix = 10;
				}
				if(digitsLeftOfDecimalPoint != -1) {
					if(!isNumber) {
						in.seek(startPos);
						return null;
					} else {
						errors.add(new BanjoParser.UnexpectedSecondDecimalPoint("Second decimal point in number", in.getFileRange(afterDigits)));
						in.seek(afterDigits);
						break;
					}
				} else {
					digitsLeftOfDecimalPoint = digits;
					cp = in.read();
				}
			} else if(isNumber && cp == '_') {
				// Allow underscore to "break up" long numbers, like in Java
				cp = in.read();
			} else if(cp == 'e' || cp == 'E') {
				// Can't start a number with an exponent
				if(!isNumber) {
					in.seek(startPos);
					return null;
				}
				// Exponent
				if(radix != 10) {
					errors.add(new BanjoParser.UnexpectedExponent("Exponent found in "+formatName+" number", in.getFileRange(afterDigits)));
					// Continue to consume any number that follow anyway, we might recover somewhat
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
					afterDigits = in.getFilePos();
					cp = in.read();
				}
				if(negexp) exp = -exp;
				break;
			} else {
				int digitValue = Character.digit(cp, radix);
				if(digitValue == -1) {
					if(!isNumber) {
						in.seek(startPos);
						return null;
					} else {
						// Number ends when we find any non-number character
						in.seek(afterDigits);
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
					afterDigits = in.getFilePos();
					cp = in.read();
				}
			}
		}
		final boolean isInteger = digitsLeftOfDecimalPoint == -1;
		final int scale = (isInteger ? 0 : digits - digitsLeftOfDecimalPoint) - exp; // TODO Check for overflow on the scale; we might have to restrict scale to 32 bits
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
		Token token = in.readTokenFrom(tokenStartPos, ignored);
		return new NumberLiteral(token, number);
	}

	/**
	 * Look at the input stream and determine if it is supposed to be a "let" expression.  If so, parse it and
	 * return a new Let.  If parsing fails after we've decided this is a "let" expression, throw a parse error.  If
	 * this does not appear to be a let expression, rewind the input and return null.
	 * 
	 * let = ID "=" Expr (";"|nl) Body
	 */
	public static Let parseLet(ParserReader in, Collection<BanjoParseException> errors) throws IOException, BanjoParseException {
		FilePos startPos = in.getFilePos();
		
		Token identifier = BanjoParser.parseID(in);
		if(identifier == null)
			return null;
		
		if(!checkOperator(in, "=")) {
			in.seek(startPos);
			return null;
		}
		
		Expr value = parseAnyExpr(in, Precedence.ASSIGNMENT, errors);
		if(!checkSeparatorOrNewline(in, ";", identifier.getFileRange().getStart().col, errors)) {
			// Variable value should be followed by semicolon or a newly indented line
			throw new BanjoParser.ExpectedSemiColonOrNewline(in.getFilePosAsRange());
		}
		Expr body = parseAnyExpr(in, Precedence.ASSIGNMENT, errors);
		
		return new Let(identifier, value, body);
	}

	public static IdRef parseIdRef(ParserReader in, Collection<BanjoParseException> errors) throws IOException {
		final Token token = BanjoParser.parseID(in);
		if(token != null) {
			return new IdRef(token, token.getText());
		} else {
			return null;
		}
	}

	/**
	 * Attempt to parse an expression in the input stream.
	 * 
	 * @param in Input code to parse
	 * @param errors Recoverable errors - i.e. looks a bit off but we can carry on anyway
	 * @return An Expr subclass for the expression that was parsed.
	 * @throws IOException If the reader throws an IOException
	 * @throws ParseException If an unrecoverable parse error occurs
	 */
	public static Expr parseAnyExpr(ParserReader in, Precedence minimumPrecedence, Collection<BanjoParseException> errors) throws IOException, BanjoParseException {
		// Atoms are always an option as they are the highest precedence
		IdRef idRef = parseIdRef(in, errors);
		if(idRef != null) return idRef;
		StringLiteral strLit = parseStringLiteral(in, errors);
		if(strLit != null) return strLit;
		NumberLiteral numLit = parseNumberLiteral(in, errors);
		if(numLit != null) return numLit;
		
		// Assignment / let
		if(minimumPrecedence.isLowerOrEqual(Precedence.ASSIGNMENT)) {
			Let node = parseLet(in, errors);
			if(node != null) return node;
		}
		
		return null;
	}
}
