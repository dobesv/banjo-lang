package banjo.parser;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.regex.Pattern;

import banjo.parser.ast.BinaryOp;
import banjo.parser.ast.Expr;
import banjo.parser.ast.Field;
import banjo.parser.ast.FunctionLiteral;
import banjo.parser.ast.IdRef;
import banjo.parser.ast.Let;
import banjo.parser.ast.ListLiteral;
import banjo.parser.ast.NumberLiteral;
import banjo.parser.ast.ObjectLiteral;
import banjo.parser.ast.Operator;
import banjo.parser.ast.Parens;
import banjo.parser.ast.Precedence;
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

	final ParserReader in;
	private final LinkedList<BanjoParseException> errors = new LinkedList<>();
	final HashMap<FilePos, String> whitespaceCache = new HashMap<>(1000);
	final HashMap<FilePos, Token> idCache = new HashMap<>(1000);
	final HashMap<FilePos, StringLiteral> stringCache = new HashMap<>(1000);
	final HashMap<FilePos, NumberLiteral> numberCache = new HashMap<>(1000);
	final HashMap<ExprCacheKey, Expr> exprCache = new HashMap<>(1000);
	
	static class ExprCacheKey {
		final FilePos startPos;
		final Precedence precedence;
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result
					+ ((precedence == null) ? 0 : precedence.hashCode());
			result = prime * result
					+ ((startPos == null) ? 0 : startPos.hashCode());
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
			ExprCacheKey other = (ExprCacheKey) obj;
			if (precedence != other.precedence)
				return false;
			if (startPos == null) {
				if (other.startPos != null)
					return false;
			} else if (!startPos.equals(other.startPos))
				return false;
			return true;
		}
		public ExprCacheKey(FilePos startPos, Precedence precedence) {
			super();
			this.startPos = startPos;
			this.precedence = precedence;
		}
		
	}
	public BanjoParser(ParserReader in) {
		super();
		this.in = in;
	}

	public BanjoParser(String inStr) {
		this(ParserReader.fromString("<string>", inStr));
	}

	public Expr parse(ParserReader in, Collection<BanjoParseException> errors) throws IOException, BanjoParseException {
		try {
			return parseAnyExpr();
		} catch(BanjoParseException pe) {
			errors.add(pe);
			return null;
		}
	}
	public static Pattern whitespace = Pattern.compile("([ \r\n]*|//[^\n]*|/\\*.*?\\*/)*", Pattern.DOTALL);
	public static Pattern idPattern = Pattern.compile("[\\p{Alpha}_-][\\p{Alnum}_-]*");
	public static Pattern opPattern = Pattern.compile("[=+-/*&^><^$#@!\\-:]+");
	public static Pattern sepPattern = Pattern.compile("[;,]");
	/**
	 * Attempt to parse an ID token; returns null on failure.
	 * 
	 * The parse position will be just after the matched identifier on success, or the
	 * original starting position on failure.
	 * @return
	 * @throws IOException 
	 */
	public Token parseID() throws IOException {
		FilePos startPos = in.getFilePos();
		if(idCache.containsKey(startPos)) {
			Token cached = idCache.get(startPos);
			if(cached != null)
				in.seek(cached.getEndPos());
			return cached;
		}
		String ignored = consumeWhitespace();
		Token result = in.checkNextToken(idPattern, ignored);
		if(result == null) {
			in.seek(startPos);
			return null;
		}
		return result;
	}

	public String consumeWhitespace() throws IOException {
		final FilePos startPos = in.getFilePos();
		String cached = whitespaceCache.get(startPos);
		if(cached != null) {
			in.skip(cached.length());
			return cached;
		}
		final String consumed = in.consume(whitespace);
		whitespaceCache.put(startPos, consumed);
		whitespaceCache.put(in.getFilePos(), ""); // Cache empty string at end of range
		return consumed;
	}

	/**
	 * Check whether the given operator follows.  If so, consume it and return true.  If not, rewind the stream to the
	 * same position as before the function was called and return false;
	 */
	public boolean checkOperator(String op, int minColumn) throws IOException {
		FilePos startPos = in.getFilePos();
		String ignored = consumeWhitespace();
		Token token = in.checkNextToken(opPattern, ignored);
		boolean result = token != null && token.getText().equals(op);
		if(!result) {
			in.seek(startPos);
		} else {
			if(in.getCurrentLineNumber() > startPos.line && in.getCurrentColumnNumber() < minColumn) {
				getErrors().add(new IncorrectIndentation(in.getFileRange(startPos), minColumn));
			}
		}
		return result;
	}

	/**
	 * Check whether the given separator follows.  If so, consume it and return true.  If not, but the next non-whitespace character
	 * is on a new line from where we started, consume the whitespace and return true.  If that fails, rewind and return false.
	 * @param sep Seperator we are looking for; e.g. ";"
	 * @param indentColumn If a newline is used as a separator, the indentation must be at least this much or a parse exception will be thrown 
	 * 
	 * @return True if the separator was found, false otherwise
	 * @throws IOException
	 * @throws IncorrectIndentation 
	 */
	public boolean checkSeparatorOrNewline(String sep, int indentColumn) throws IOException {
		FilePos startPos = in.getFilePos();
		String ignored = consumeWhitespace();
		FilePos tokenStartPos = in.getFilePos();
		Token token = in.checkNextToken(sepPattern, ignored);
	    boolean result = token != null && token.getText().equals(sep);
	    if(!result) {
	    	if(tokenStartPos.line > startPos.line) {
	    		in.seek(tokenStartPos);
	    		if(tokenStartPos.column != indentColumn)
	    			getErrors().add(new IncorrectIndentation(in.getFileRange(in.getFilePos().lineStart()), indentColumn));
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
		public int getStartColumn() { return range.getStart().column; }
		public int getEndLine() { return range.getEnd().line; }
		public int getEndColumn() { return range.getEnd().column; }
	}
	public static class ExpectedExpression extends BanjoParseException {
		private static final long serialVersionUID = 1L;

		public ExpectedExpression(FileRange fileRange) {
			super("Invalid expression after this position", fileRange);
		}
	}
	public static class ExpectedElement extends BanjoParseException {
		private static final long serialVersionUID = 1L;
	
		public ExpectedElement(FileRange range) {
			super("Expected comma, newline, or ']'", range);
		}
	}
	public static class ExpectedField extends BanjoParseException {
		private static final long serialVersionUID = 1L;
	
		public ExpectedField(FileRange range) {
			super("Expected key : value pair or '}'", range);
		}
	}

	public static class ExpectedFunctionArgsBodySeparator extends BanjoParseException {
		private static final long serialVersionUID = 1L;
	
		public ExpectedFunctionArgsBodySeparator(String message, FileRange range) {
			super(message, range);
		}
	
		public ExpectedFunctionArgsBodySeparator(int followedBy, FileRange fileRange) {
			this(new StringBuffer().append("Expected function argument list to be separated from body with '").appendCodePoint(followedBy).append("'").toString(), fileRange);
		}
	
	}
	public static class ExpectedColon extends BanjoParseException {
		private static final long serialVersionUID = 1L;
	
		public ExpectedColon(String key, FileRange range) {
			super("Expected ':' after key '"+key+"'", range);
		}
	}
	
	public static class UnexpectedCloseParen extends BanjoParseException {
		private static final long serialVersionUID = 1L;
	
		public UnexpectedCloseParen(FileRange range) {
			super("Unexpected ')'", range);
		}
	
	}
	public static class MissingCloseParen extends BanjoParseException {
		private static final long serialVersionUID = 1L;
	
		public MissingCloseParen(FileRange range) {
			super("Missing ')'", range);
		}
	
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
			this(range, indentColumn, false);
		}
		public IncorrectIndentation(FileRange range, int indentColumn, boolean orMore) {
			super("Expected indentation to column "+indentColumn+(orMore?" or more":"")+" but indentation was "+range.getEnd().column+" columns.", range);
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
		}
	
	}
	public static class UnexpectedSecondDecimalPoint extends UnexpectedDecimalPoint {
		private static final long serialVersionUID = 1L;

		public UnexpectedSecondDecimalPoint(String message, FileRange range) {
			super(message, range);
		}
	
	}

	public static class MissingWhitespace extends BanjoParseException {
		private static final long serialVersionUID = 1L;

		public MissingWhitespace(String message, FileRange range) {
			super(message, range);
		}
	
	}
	/**
	 * Generic kind of "I have no idea what you've typed in here" error.
	 */
	public static class SyntaxError extends BanjoParseException {
		private static final long serialVersionUID = 1L;
	
		public SyntaxError(FileRange range) {
			super("Syntax error", range);
		}
	
	}

	/**
	 * Skip whitespace and attempt to parse the next token by matching the given regular
	 * expression.  If the regular expression doesn't match, rewaind to before any skipped
	 * whitespace and return null.  If it does match, consume the token and return it.
	 */
	public Token checkPattern(Pattern p) throws IOException {
		FilePos startPos = in.getFilePos();
		String ignored = consumeWhitespace();
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
	
	public StringLiteral parseStringLiteral() throws IOException {
		FilePos startPos = in.getFilePos();
		if(stringCache.containsKey(startPos)) {
			StringLiteral cached = stringCache.get(startPos);
			if(cached != null)
				in.seek(cached.getFileRange().getEnd());
			return cached;
		}
		
		String ignored = consumeWhitespace();
		
		int cp = in.read();
		if(cp != '"' && cp != '\'') {
			in.seek(startPos);
			return null;
		}
		int quoteType = cp;
			
		FilePos tokenStartPos = in.getFilePos();
	    StringBuffer buf = new StringBuffer(in.remaining());
	    while((cp = in.read()) != -1) {
	    	if(cp == quoteType)
	    		break; // End of string
	        if(cp != '\\') {
	            buf.appendCodePoint(cp);
	            continue;
	        }
	        FilePos afterBackslash = in.getFilePos();
	        cp = in.read();
	        if(cp == -1) {
	            getErrors().add(new BadStringEscapeSequence("Backslash at end of string.", in.getFileRange(tokenStartPos))); // Shouldn't normally be possible
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
	            	readOctalEscape(cp, buf);
	                break;
	            }
	
	            case 'x':  {
	            	readHexEscape(buf, 2);
	                break;
	            }
	
	            case 'u': {
	            	readHexEscape(buf, 4);
	                break;
	            }
	
	            default:  {
	                getErrors().add(new BadStringEscapeSequence("Unknown escape sequence "+new String(Character.toChars(cp)), in.getFileRange(afterBackslash)));
	                break;
	            }
	        }
	    }
	    if(cp == -1) getErrors().add(new BanjoParser.PrematureEndOfFile("End of file in string literal", in.getFileRange(tokenStartPos)));
		return new StringLiteral(in.readTokenFrom(tokenStartPos, ignored), buf.toString());
	}

	private void readHexEscape(StringBuffer buf, final int digitCount) throws IOException {
		int result = 0;
		FilePos afterDigits = in.getFilePos();
		for(int digits = 0; digits < digitCount; digits++) {
			int digitValue = Character.digit(in.read(), 16);
			if(digitValue == -1) {
		    	getErrors().add(new BadStringEscapeSequence("Invalid hex digit in \\x escape", in.getFileRange(afterDigits)));
		    	in.seek(afterDigits);
		    	return;
			} else {
				result = (result << 4) | digitValue;
				afterDigits = in.getFilePos();
			}
		}
		buf.appendCodePoint(result);
	}

	private void readOctalEscape(int cp, StringBuffer buf) throws IOException {
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

	public NumberLiteral parseNumberLiteral() throws IOException {
		FilePos startPos = in.getFilePos();
		if(numberCache.containsKey(startPos)) {
			NumberLiteral cached = numberCache.get(startPos);
			if(cached != null)
				in.seek(cached.getFileRange().getEnd());
			return cached;
		}
		String ignored = consumeWhitespace();
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
					getErrors().add(new BanjoParser.UnexpectedDecimalPoint("Decimal point found in "+formatName+" number", in.getFileRange(afterDigits)));
					radix = 10;
				}
				if(digitsLeftOfDecimalPoint != -1) {
					if(!isNumber) {
						in.seek(startPos);
						numberCache.put(startPos, null);
						return null;
					} else {
						getErrors().add(new BanjoParser.UnexpectedSecondDecimalPoint("Second decimal point in number", in.getFileRange(afterDigits)));
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
					numberCache.put(startPos, null);
					return null;
				}
				// Exponent
				if(radix != 10) {
					getErrors().add(new BanjoParser.UnexpectedExponent("Exponent found in "+formatName+" number", in.getFileRange(afterDigits)));
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
						numberCache.put(startPos, null);
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
		final NumberLiteral result = new NumberLiteral(token, number);
		numberCache.put(startPos, result);
		return result;
	}

	/**
	 * Look at the input stream and determine if it is supposed to be a "let" expression.  If so, parse it and
	 * return a new Let.  If parsing fails after we've decided this is a "let" expression, throw a parse error.  If
	 * this does not appear to be a let expression, rewind the input and return null.
	 * 
	 * let = ID "=" Expr (";"|nl) Body
	 */
	public Let parseLet() throws IOException, BanjoParseException {
		FilePos startPos = in.getFilePos();
		
		Token identifier = parseID();
		if(identifier == null)
			return null;
		
		final int idStartColumn = identifier.getFileRange().getStart().column;
		if(!checkOperator("=", idStartColumn)) {
			in.seek(startPos);
			return null;
		}
		
		Expr value = parseAnyExpr();
		
		// We expect an expression after the "="
		if(value == null) {
			throw new ExpectedExpression(in.getFilePosAsRange());
		}
		
		// Value should be indented past the identifier
		if(value.getStartColumn() <= idStartColumn) {
			errors.add(new IncorrectIndentation(value.getFileRange(), idStartColumn+1, true));
		}
		
		// Variable value should be followed by semicolon or a newly indented line at the same
		// column position as the start of the identifier
		if(!checkSeparatorOrNewline(";", idStartColumn)) {
			throw new BanjoParser.ExpectedSemiColonOrNewline(in.getFilePosAsRange());
		}
		Expr body = parseAnyExpr();
		if(body == null) {
			throw new ExpectedExpression(in.getFilePosAsRange());
		}
		return new Let(identifier, value, body);
	}

	/**
	 * Parse a simple identifier expression.  If the current parse position has an
	 * identifier, this consumes it and returns it.  Otherwise, this resets the parse
	 * position and returns null.
	 * 
	 * @return An IdRef if the parse is successful; null otherwise.
	 * @throws IOException
	 */
	public IdRef parseIdRef() throws IOException {
		final Token token = parseID();
		if(token != null) {
			return new IdRef(token, token.getText());
		} else {
			return null;
		}
	}

	/**
	 * Attempt to parse an expression in the input stream.
	 * 
	 * @return An Expr subclass for the expression that was parsed.
	 * @throws IOException If the reader throws an IOException
	 * @throws ParseException If an unrecoverable parse error occurs
	 */
	public Expr parseAnyExpr(Precedence precedence) throws IOException, BanjoParseException {
		final FilePos startPos = in.getFilePos();
		Expr result = null;
		ExprCacheKey cacheKey = new ExprCacheKey(startPos, precedence);
		if(exprCache.containsKey(cacheKey)) {
			Expr cached = exprCache.get(cacheKey);
			if(cached != null)
				in.seek(cached.getFileRange().getEnd());
			return cached;
		}
		
		
		// Work our way from lowest precedence to highest
		// Assignment / let
		if(precedence == Precedence.ATOM) {
			result = parseIdRef();
			if(result == null) {
				result = parseStringLiteral();
				if(result == null) {
					result = parseNumberLiteral();
					if(result == null) {
						result = parseParentheses();
						if(result == null) {
							result = parseListLiteral();
							if(result == null) {
								result = parseObjectLiteral();
							}
						}
					}
				}
			}
		} else {
			if(precedence == Precedence.ASSIGNMENT) {
				result = parseLet();
			} else if(precedence == Precedence.TERNARY) {
				// TODO Ternary (conditional) operator
			} else {
				// Binary ops
				for(Operator operator : Operator.values()) {
					if(operator.getPrecedence() == precedence) {
						result = parseBinaryOp(operator);
						if(result != null)
							break;
					}
				}
				// Binary ops didn't work?  try unary ops ...
				if(result == null) {
					
				}
			}
			
			// If we don't find what we're looking for at this level, try again at a higher level.
			if(result == null) {
				result = parseAnyExpr(precedence.nextHighest());
			}
		}
		exprCache.put(cacheKey, result);
		return result;
	}

	private Expr parseParentheses() throws BanjoParseException, IOException {
		FilePos startPos = in.getFilePos();
		FilePos openParenPos = consumeWhitespaceGetFilePos();
		if(in.read() == '(') {
			Expr expr = parseAnyExpr();
			
			consumeWhitespace();
			if(in.read() == ')') {
				return new Parens(in.getFileRange(openParenPos), expr);
			}
		}
		in.seek(startPos);
		return null;
	}

	public FilePos consumeWhitespaceGetFilePos() throws IOException {
		consumeWhitespace();
		return in.getFilePos();
	}

	public Expr parseAnyExpr() throws IOException, BanjoParseException {
		return parseAnyExpr(Precedence.lowest());
	}

	/**
	 * Parse a binary operator, like binary multiplication, division, and so on.
	 * 
	 * @param operator Operator we're looking for now
	 * @return
	 * @throws IOException 
	 * @throws BanjoParseException 
	 */
	public BinaryOp parseBinaryOp(Operator operator) throws BanjoParseException, IOException {
		FilePos startPos = in.getFilePos();
		consumeWhitespace();
		Expr left = parseAnyExpr(operator.getPrecedence().nextHighest());
		if(left == null) {
			in.seek(startPos);
			return null;
		}
		boolean foundOp = checkOperator(operator.getOp(), left.getStartColumn());
		if(!foundOp) {
			in.seek(startPos);
			return null;
		}
		Expr right = parseAnyExpr(operator.getPrecedence());
		if(right == null) {
			in.seek(startPos);
			return null;
		}
		return new BinaryOp(operator, left, right);
	}
	
	static final int BULLET = '\u2022';
	
	/**
	 * Parse a list literal.  A list literal comes in two forms:
	 * 
	 * <ul>
	 * <li><code>[x,y,z]</code> - A comma-separated list surrounded with square brackets</li>
	 * <li>A series of bullets, aligned vertically:
	 *     <ul><li>x</li><li>y</li><li>z</li></ul></li>
	 * </ul>
	 */
	public ListLiteral parseListLiteral() throws IOException, BanjoParseException {
		FilePos startPos = in.getFilePos();
		FilePos listStartPos = consumeWhitespaceGetFilePos();
		int cp = in.read();
		ArrayList<Expr> elements = new ArrayList<>();
		if(cp == BULLET) {
			int bulletColumn = listStartPos.column;
			for(;;) {
				String ws = consumeWhitespace();
				if(ws.isEmpty()) {
					getErrors().add(new MissingWhitespace("Expected whitespace after bullet.", in.getFilePosAsRange()));
				}
				Expr elt = parseAnyExpr();
				if(elt == null) {
					// TODO We could try to find a matching bullet for the next element and mark everything in between as a recoverable error
					throw new SyntaxError(in.getFilePosAsRange());
				} else {
					elements.add(elt);
				}
				
				FilePos afterElements = in.getFilePos();
				consumeWhitespace();
				int col = in.getCurrentColumnNumber();
				if(col < bulletColumn) {
					// Dedent, so we're done
					in.seek(afterElements);
					break;
				}
				cp = in.read();
				if(cp != BULLET) {
					in.seek(afterElements);
					break;
				}
				if(col > bulletColumn) {
					// Unexpected indent; most likely explanation is a missing operator of some sort; we should abort
					throw new IncorrectIndentation(in.getFilePosAsRange(), bulletColumn);
				}
			}
			return new ListLiteral(in.getFileRange(listStartPos), elements);
		} else if(cp == '[') {
			consumeWhitespace();
			int eltColumn = in.getCurrentColumnNumber();
			for(;;) {
				Expr elt = parseAnyExpr();
				if(elt == null) {
					consumeWhitespace();
					cp = in.read();
					if(cp == ',') {
						continue;
					}
					if(cp == ']') {
						break;
					}
					// TODO We could try to find a matching comma for the next element and mark everything in between as a recoverable error of sorts
					throw new SyntaxError(in.getFilePosAsRange());
				} else {
					elements.add(elt);
				}
				FilePos prevElementEnd = in.getFilePos();
				consumeWhitespace();
				final boolean newLine = prevElementEnd.line != in.getCurrentLineNumber();
				if(newLine) {
					if(in.getCurrentColumnNumber() != eltColumn)
						getErrors().add(new IncorrectIndentation(in.getFileRange(prevElementEnd), eltColumn));	// All the list elements should be lined up vertically if they are not on the same line
					continue;
				}
				cp = in.read();
				if(cp == ']') {
					break;
				}
				if(cp == ',') {
					consumeWhitespace();
					boolean newLineAfterComma = in.getCurrentLineNumber() > prevElementEnd.line;
					if(newLineAfterComma && in.getCurrentColumnNumber() != eltColumn) {
						getErrors().add(new IncorrectIndentation(in.getFileRange(prevElementEnd), eltColumn));
					}
					continue;
				}
				// Missing separator - should be either a newline or a comma between elements
				throw new ExpectedElement(in.getFilePosAsRange());
			}
			return new ListLiteral(in.getFileRange(listStartPos), elements);
		} else {
			in.seek(startPos);
			return null;
		}
	}
	
	/**
	 * Parse an object literal.  An object literal comes in two forms:
	 * 
	 * k1: value1
	 * k2: value2
	 * 
	 * or
	 * 
	 * { k1: value1, k2:value2 }
	 * 
	 * In the second form, commas are optional if there is a newline and indent to the same column as the first key on the first line.
	 * 
	 * @return A newly parsed object literal expression, or null if the stream doesn't look like an object
	 * @throws IOException
	 * @throws BanjoParseException In case of a 
	 */
	public ObjectLiteral parseObjectLiteral() throws IOException, BanjoParseException {
		FilePos startPos = in.getFilePos();
		LinkedHashMap<String,Field> fields = new LinkedHashMap<>();
		
		Token identifier = parseID();
		if(identifier != null) {
			FilePos objectStart = identifier.getFileRange().getStart();
			if(!checkOperator(":", identifier.getFileRange().getStart().column)) {
				in.seek(startPos);
				return null;
			}
			
			// Looks like a key/value pair to me!
			int keyColumn = identifier.getStartColumn();
			for(;;) {
				Expr valueExpr = parseAnyExpr();
				if(valueExpr == null) {
					// TODO We could try to find a matching bullet for the next element and mark everything in between as an error
					throw new SyntaxError(in.getFilePosAsRange());
				} else {
					fields.put(identifier.getText(), new Field(identifier, valueExpr));
				}
				
				FilePos afterField = in.getFilePos();
				consumeWhitespace();
				identifier = parseID();
				if(identifier == null || identifier.getStartColumn() < keyColumn || !checkOperator(":", identifier.getFileRange().getStart().column)) {
					// Doesn't look like we have another field coming ...
					in.seek(afterField);
					return new ObjectLiteral(in.getFileRange(objectStart), fields);
				}
				if(identifier.getStartColumn() > keyColumn) {
					// Unexpected indent; most likely explanation is a missing operator of some sort; we should abort
					throw new IncorrectIndentation(in.getFilePosAsRange(), keyColumn);
				}
			}
		}
		
		// Not a vertical one, perhaps it is a {} style object
		in.seek(startPos);
		FilePos objectStartPos = consumeWhitespaceGetFilePos();
		int cp = in.read();
		if(cp == '{') {
			consumeWhitespace();
			int eltColumn = in.getCurrentColumnNumber();
			for(;;) {
				identifier = parseID();
				if(identifier == null) {
					consumeWhitespace();
					cp = in.read();
					if(cp == ',') {
						continue; // Ignoring extra commas here, not sure if that is wise
					}
					if(cp == '}') {
						break;
					}
					if(fields.isEmpty()) {
						in.seek(startPos);
						return null;
					} else {
						throw new ExpectedField(in.getFilePosAsRange());
					}
				}
				final int idStartColumn = identifier.getFileRange().getStart().column;
				if(!checkOperator(":", idStartColumn)) {
					throw new ExpectedColon(identifier.getText(), in.getFilePosAsRange());
				}
				
				// Looks like a key/value thing
				Expr valueExpr = parseAnyExpr();
				if(valueExpr == null) {
					// TODO We could try to find a matching comma for the next element and mark everything in between as a recoverable error of sorts
					throw new SyntaxError(in.getFilePosAsRange());
				} else {
					fields.put(identifier.getText(), new Field(identifier, valueExpr));
				}
				FilePos prevElementEnd = in.getFilePos();
				consumeWhitespace();
				final boolean newLine = prevElementEnd.line < in.getCurrentLineNumber();
				if(newLine) {
					if(in.getCurrentColumnNumber() != eltColumn)
						getErrors().add(new IncorrectIndentation(in.getFileRange(prevElementEnd), eltColumn));	// All the list elements should be lined up vertically if they are not on the same line
					continue;
				}
				cp = in.read();
				if(cp == '}') {
					break;
				}
				if(cp == ',') {
					consumeWhitespace();
					boolean newLineAfterComma = in.getCurrentLineNumber() > prevElementEnd.line;
					if(newLineAfterComma && in.getCurrentColumnNumber() != eltColumn) {
						getErrors().add(new IncorrectIndentation(in.getFileRange(prevElementEnd), eltColumn));
					}
					continue;
				}
				// Missing separator - should be either a newline or a comma between elements
				throw new ExpectedField(in.getFilePosAsRange());
			}
			return new ObjectLiteral(in.getFileRange(objectStartPos), fields);
		} else {
			in.seek(startPos);
			return null;
		}
	}
	
	static final int LITERAL_FUNCTION_ARROW = 0x21a6;
	/**
	 * <p>Parse a function literal.  A function literal looks roughly like: </p>
	 *  
	 * <p>Shortest form:
	 * 
	 * <ul><li><code>a↦a*a</code></li>
	 *     <li><code>a,b↦a+b</code></li></ul>
	 * </p>
	 * <p> Arguments can be surrounded with parentheses:
	 * 
	 * <ul><li><code>(a)↦a*a</code></li>
	 *     <li><code>(a,b)↦a+b</code></li></ul>
	 * </p>
	 * 
	 * <p>A function with no arguments (aka a lazy value) can omit the argument list:
	 * 
	 * <ul><li><code>↦ x+5</code></li></ul>
	 * </p>
	 * 
	 * <p> Notes:
	 * <ul><li>Arrow (->) is unicode 0x2192.  In mathematical notation the arrow represents the domain and range of a function</li>  
	 *     <li>Arrow from bar (|->) is 0x21a6.  In mathematical notation this is used to describe the computation of a function</li></ul>
	 * </p>
	 * 
	 * @return A new FunctionLiteral if found, otherwise null
	 * @throws IOException If there's a problem reading the underlying stream
	 * @throws BanjoParseException If there's a non-recoverable parse error
	 */
	public FunctionLiteral parseFunctionLiteral() throws IOException, BanjoParseException {
		FilePos startPos = in.getFilePos();
		FilePos functionStartPos = consumeWhitespaceGetFilePos();
		LinkedHashMap<String, Token> args = parseArgumentList(LITERAL_FUNCTION_ARROW);
		if(args == null) {
			// Check for a lazy value (i.e. missing parameter list)
			if(in.read() == LITERAL_FUNCTION_ARROW) {
				args = new LinkedHashMap<>(); // Empty argument list in this case
			} else {
				in.seek(startPos);
				return null;
			}
		}
		Expr body = parseAnyExpr();
		if(body == null) {
			throw new ExpectedExpression(in.getFilePosAsRange());
		}
		return new FunctionLiteral(in.getFileRange(functionStartPos), args, body);
	}

	/**
	 * <p> Parse a function argument list.  It may or may not be surrounded by parentheses.
	 * 
	 * <p> If the input looks like a function argument list, this returns it.  Otherwise, the input stream is reset
	 * to the same as before the call and this returns null.
	 * 
	 * <p> This is shared between the code to parse a let and to parse a function literal.
	 * 
	 * <p> This will not detect an empty parameter list unless there are parentheses around it.  For lazy value syntax,
	 *     the caller should check if this fails (or even before calling this, depending). </p>
	 * @param followedBy Expect this character to follow the argument list; would be '=' for a let, or arrow for a function literal.
	 *     
	 * @return Parsed function argument list.
	 * @throws IOException 
	 * @throws SyntaxError 
	 */
	private LinkedHashMap<String, Token> parseArgumentList(int followedBy) throws IOException {
		FilePos startPos = in.getFilePos();
		consumeWhitespace();
		int startChar = in.read();
		boolean foundOpenParen = (startChar == '(');
		if(!foundOpenParen) {
			in.seek(startPos);
		}
		
		Token id = parseID();
		if(id == null) {
			// Possibly an empty pair of parens: (); if the right separator follows it, then accept it as a parameter list
			if(foundOpenParen) {
				consumeWhitespace();
				if(in.read() == ')') {
					consumeWhitespace();
					if(in.read() == followedBy) {
						return new LinkedHashMap<>();
					}
				}
			}
			in.seek(startPos);
			return null;
		}
		LinkedHashMap<String, Token> args = new LinkedHashMap<>();
		for(;;) {
			args.put(id.getText(), id);
			
			FilePos afterArg = in.getFilePos();
			
			// We read an argument name - now check for a comma or newline 
			if(!checkSeparatorOrNewline(",", id.getStartColumn()) || (id = parseID()) == null) {
				
				// No comma or no newline or no ID following those. Perhaps we'll find that separator we're looking for, or a close paren
				consumeWhitespace();
				int cp = in.read();
				if(foundOpenParen && cp == ')') {
					FilePos afterCloseParen = in.getFilePos();
					consumeWhitespace();
					cp = in.read();
					if(cp != followedBy) {
						// All good!
						getErrors().add(new ExpectedFunctionArgsBodySeparator(followedBy, in.getFileRange(afterCloseParen)));
						in.seek(afterCloseParen);
					}
					return args;
				} else if(cp == followedBy) {
					if(foundOpenParen) {
						getErrors().add(new MissingCloseParen(in.getFileRange(afterArg)));
					}
					return args;
				} else if(foundOpenParen && args.size() > 1) {
					// If there's an open parenthesis and two or more elements then we can assume this is
					// an argument list of some sort and report errors.
					// This might be a forgotten close parenthesis but it might also be a badly formed
					// argument.
					// TODO Could be smarter here - scan ahead for the close paren or the arrow, maybe
					getErrors().add(new MissingCloseParen(in.getFileRange(afterArg)));
					getErrors().add(new ExpectedFunctionArgsBodySeparator(followedBy, in.getFileRange(afterArg)));
					return args;
				} else {
					// Probably not an argument list after all
					in.seek(startPos);
					return null;
				}
			}
		}
	}

	public Collection<BanjoParseException> getErrors() {
		return errors;
	}
}
