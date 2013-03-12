package banjo.parser;

import java.io.EOFException;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Pattern;

import banjo.parser.ast.BinaryOp;
import banjo.parser.ast.BinaryOperator;
import banjo.parser.ast.Call;
import banjo.parser.ast.Cond;
import banjo.parser.ast.CondCase;
import banjo.parser.ast.Ellipsis;
import banjo.parser.ast.Expr;
import banjo.parser.ast.Field;
import banjo.parser.ast.FieldRef;
import banjo.parser.ast.FunArg;
import banjo.parser.ast.FunctionLiteral;
import banjo.parser.ast.IdRef;
import banjo.parser.ast.Let;
import banjo.parser.ast.ListLiteral;
import banjo.parser.ast.Lookup;
import banjo.parser.ast.NumberLiteral;
import banjo.parser.ast.ObjectLiteral;
import banjo.parser.ast.ParenType;
import banjo.parser.ast.Parens;
import banjo.parser.ast.Precedence;
import banjo.parser.ast.Steps;
import banjo.parser.ast.StringLiteral;
import banjo.parser.ast.StringLiteral.BadStringEscapeSequence;
import banjo.parser.ast.UnaryOp;
import banjo.parser.ast.UnaryOperator;
import banjo.parser.ast.UnitRef;
import banjo.parser.errors.BanjoParseException;
import banjo.parser.errors.ExpectedCase;
import banjo.parser.errors.ExpectedCloseBrace;
import banjo.parser.errors.ExpectedCloseBracket;
import banjo.parser.errors.ExpectedCloseParen;
import banjo.parser.errors.ExpectedElement;
import banjo.parser.errors.ExpectedExpression;
import banjo.parser.errors.ExpectedField;
import banjo.parser.errors.ExpectedFieldName;
import banjo.parser.errors.ExpectedIdentifier;
import banjo.parser.errors.ExpectedOperator;
import banjo.parser.errors.IncorrectIndentation;
import banjo.parser.errors.MixedSemicolonAndComma;
import banjo.parser.errors.PrematureEndOfFile;
import banjo.parser.errors.SyntaxError;
import banjo.parser.errors.UnexpectedCloseParen;
import banjo.parser.errors.UnexpectedDecimalPoint;
import banjo.parser.errors.UnexpectedExponent;
import banjo.parser.errors.UnexpectedSecondDecimalPoint;
import banjo.parser.errors.UnsupportedBinaryOperator;
import banjo.parser.errors.UnsupportedUnaryOperator;
import banjo.parser.util.FilePos;
import banjo.parser.util.FileRange;
import banjo.parser.util.ParserReader;
import banjo.parser.util.ParserReader.Pos;
import banjo.parser.util.Token;

/**
 * Change input into an AST.
 */
public class BanjoParser {

	final ParserReader in;
	private final LinkedList<BanjoParseException> errors = new LinkedList<>();

	public BanjoParser(ParserReader in) {
		super();
		this.in = in;
	}

	public BanjoParser(String inStr) {
		this(ParserReader.fromString("<string>", inStr));
	}

	public static Pattern whitespace = Pattern.compile("([ \r\n]*|//[^\n]*|/\\*.*?\\*/)*", Pattern.DOTALL);
	public static Pattern idPattern = Pattern.compile("[\\p{Alpha}_-][\\p{Alnum}_-]*");
	public static Pattern opPattern = Pattern.compile("[=+-/*&^><!\\-:]+");
	public static Pattern sepPattern = Pattern.compile("[;,]");
	
	static boolean isIdentifierStart(int cp) {
		return cp == '_' || cp == '$' || Character.isLetter(cp);
	}
	
	static final int NBSP = '\u00A0';
	static final int NNBSP = '\u202F';
	static final int ELLIPSIS = '\u2026';
	
	
	static boolean isIdentifierPart(int cp) {
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
	 * @return
	 * @throws IOException
	 */
	public String matchID() throws IOException {
		final int first = in.read();
		if(!isIdentifierStart(first)) {
			in.unread();
			return null;
		}
		buf.setLength(0); // Reset buffer
		buf.appendCodePoint(first);
		for(;;) {
			int cp = in.read();
			if(!isIdentifierPart(cp)) {
				in.unread();
				return buf.toString();
			}
			buf.appendCodePoint(cp);
		}
	}
	
	public static final boolean isOperatorChar(int codePoint) {
		switch(codePoint) {
		case '"': 
		case '\'':
		case '_':
		case '.': // Handled specially
			return false;
		case '-':
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
	public String matchOperator() throws IOException {
		int first = in.read();
		// These separators are always taken one at a time
		if(first == ';' || first == ',') {
			return Character.toString((char)first);
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
	final private Pos commentStart = new Pos();
	public void skipWhitespace() throws IOException {
		for(;;) {
			int cp = in.read();
			if(!isWhitespaceChar(cp))
				break;
			if(cp == '/') {
				in.getPreviousPosition(commentStart);
				cp = in.read();
				if(cp == '/') {
					skipToEndOfLine();
				} else if(cp == '*') {
					skipToEndOfMultilineComment();
				} else {
					// Not a comment, and thus also not whitespace any more
					in.seek(commentStart);
					return;
				}
			}
			// continue
		}
		in.unread(); // Push back the non-whitespace character we found
	}
	public void skipWhitespace(Pos posAfter) throws IOException {
		skipWhitespace();
		in.getCurrentPosition(posAfter);
	}

	/**
	 * Assuming we just passed a '/' and a '*', read until we find the matching 
	 * '*' and '/' sequence.
	 */
	public void skipToEndOfMultilineComment() throws IOException {
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
	public void skipToEndOfLine() throws IOException {
		int currentLine = in.getCurrentLineNumber();
		while(in.read() != -1 && in.getCurrentLineNumber() == currentLine) {
			// continue
		}
	}

	/**
	 * Check whether the given operator follows.  If so, consume it and return true.  If not, rewind the stream to the
	 * same position as before the function was called and return false;
	 */
	public boolean checkOperator(String op, int minColumn) throws IOException {
		FilePos startPos = in.getFilePos();
		skipWhitespace();
		Token token = in.checkNextToken(opPattern);
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
		skipWhitespace();
		FilePos tokenStartPos = in.getFilePos();
		Token token = in.checkNextToken(sepPattern);
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
	
	/**
	 * Skip whitespace and attempt to parse the next token by matching the given regular
	 * expression.  If the regular expression doesn't match, rewaind to before any skipped
	 * whitespace and return null.  If it does match, consume the token and return it.
	 */
	public Token checkPattern(Pattern p) throws IOException {
		FilePos startPos = in.getFilePos();
		skipWhitespace();
		Token tok = in.checkNextToken(p);
		if(tok == null)
			in.seek(startPos);
		return tok;
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
	 *
	 * TODO: Error reporting ...
	 */
	
	public StringLiteral parseStringLiteral() throws IOException {
		in.getCurrentPosition(tokenStartPos);
		
		int cp = in.read();
		if(cp != '"' && cp != '\'') {
			in.seek(tokenStartPos);
			return null;
		}
		int quoteType = cp;
			
		
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
	    if(cp == -1) getErrors().add(new PrematureEndOfFile("End of file in string literal", in.getFileRange(tokenStartPos)));
		return new StringLiteral(in.readTokenFrom(tokenStartPos), buf.toString());
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
					getErrors().add(new UnexpectedDecimalPoint("Decimal point found in "+formatName+" number", in.getFileRange(afterDigits)));
					radix = 10;
				}
				if(digitsLeftOfDecimalPoint != -1) {
					if(!isNumber) {
						in.seek(tokenStartPos);
						return null;
					} else {
						getErrors().add(new UnexpectedSecondDecimalPoint("Second decimal point in number", in.getFileRange(afterDigits)));
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
					return null;
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
						in.seek(tokenStartPos);
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
		Token token = in.readTokenFrom(tokenStartPos);
		final NumberLiteral result = new NumberLiteral(token, number);
		return result;
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
		in.getCurrentPosition(tokenStartPos);
		String identifier = matchID();
		if(identifier == null)
			return null;
		FileRange identRange = in.getFileRange(tokenStartPos);
		return new IdRef(identRange, identifier);
	}
	
	/**
	 * Check for an Eliipsis at the current parse position.
	 */
	public Ellipsis parseEllipsis() throws IOException {
		if(in.checkNextChar('.')) {
			in.getCurrentPosition(tokenStartPos);
			if(in.read() == '.' && in.read() == '.') {
				return new Ellipsis(in.getFileRange(tokenStartPos));
			}
			in.seek(tokenStartPos);
		} else if(in.checkNextChar(ELLIPSIS)) {
			return new Ellipsis(in.getFileRange(in.getPreviousPosition(tokenStartPos)));
		}
		return null;
	}

	class PartialBinaryOp {
		final BinaryOperator operator;
		private final Expr operand;
		public PartialBinaryOp(BinaryOperator operator, Expr operand) {
			super();
			this.operator = operator;
			this.operand = operand;
		}
		public BinaryOp makeOp(Expr secondOperand) {
			// The second operand must be indented to at least the same position as the first
			if(secondOperand.getStartColumn() < getStartColumn()) {
				errors.add(new IncorrectIndentation(secondOperand.getFileRange(), getStartColumn(), true));
			}
			return new BinaryOp(operator, getOperand(), secondOperand);
		}
		public Precedence getPrecedence() {
			return operator.getPrecedence();
		}
		public int getStartColumn() {
			return getOperand().getStartColumn();
		}
		@Override
		public String toString() {
			return "(" + getOperand().toSource()+" "+operator.getOp() +" _)";
		}
		public BinaryOperator getOperator() {
			return operator;
		}
		public Expr getOperand() {
			return operand;
		}
	}
	
	class PartialUnaryOp {
		final UnaryOperator operator;
		final FileRange range;
		public PartialUnaryOp(UnaryOperator operator, FileRange range) {
			super();
			this.operator = operator;
			this.range = range;
		}
		public UnaryOp makeOp(Expr operand) {
			if(operand.getStartColumn() < getStartColumn()) {
				// Operand should be at the same level or higher indentation as the unary operator itself
				errors.add(new IncorrectIndentation(operand.getFileRange(), getStartColumn(), true));
			}
			return new UnaryOp(new FileRange(range,operand.getFileRange()), operator, operand);
		}
		public int getStartColumn() {
			return range.getStart().getColumn();
		}
		public Precedence getPrecedence() {
			return operator.getPrecedence();
		}
		
		@Override
		public String toString() {
			return "(+ _)";
		}
	}
	
	/**
	 * shunting-yard algorithm
	 */
	
	public Expr parseExpr() throws IOException, BanjoParseException {
		LinkedList<PartialBinaryOp> binaryOpStack = new LinkedList<>();
		LinkedList<PartialUnaryOp> unaryOpStack = new LinkedList<>();
		Pos beforeToken = new Pos();
		parseExprLoop: for(;;) {
			skipWhitespace(beforeToken);
			
			String unaryOp = matchOperator();
			if(unaryOp != null) {
				UnaryOperator operator = UnaryOperator.fromOp(unaryOp);
				if(operator != null) {
					unaryOpStack.push(new PartialUnaryOp(operator, in.getFileRange(beforeToken)));
				} else {
					errors.add(new UnsupportedUnaryOperator(unaryOp, in.getFileRange(beforeToken)));
				}
				continue;
			}
			Expr operand;
			if((operand = parseIdRef()) == null &&
			   (operand = parseStringLiteral()) == null &&
		  	   (operand = parseNumberLiteral()) == null && 
		  	   (operand = parseParentheses()) == null &&
		  	   (operand = parseEllipsis()) == null) {
				
				// Allow trailing comma / semicolon before close paren
				int codePoint = in.peek();
				final boolean trailingSeparator = isCloseParenOrEof(codePoint) &&
					!binaryOpStack.isEmpty() && 
					isListSeparator(binaryOpStack.getFirst().getOperator());
				if(trailingSeparator) {
					operand = binaryOpStack.pop().getOperand();
				} else if(codePoint == -1) {
					throw new PrematureEndOfFile("Unexpected end of file/input", in.getFilePosAsRange());
				} else if(isCloseParen(codePoint)) {
					errors.add(new UnexpectedCloseParen(in.getFileRange(beforeToken)));
				} else {
					String butGot = matchOperator();
					if(butGot == null) {
						try {
							butGot = in.readString(1);
						} catch(EOFException e) {
							butGot = "<EOF>";
						}
					}
					errors.add(new ExpectedExpression(in.getFileRange(beforeToken), butGot));
				}
				if(operand == null) {
					if(!binaryOpStack.isEmpty()) {
						operand = binaryOpStack.pop().getOperand();
					} else {
						operand = new UnitRef(in.getFileRange(beforeToken), ParenType.BRACES);
					}
				}
			}
			
			// a + b
			// c + d 
			// Parse suffixes like '.', call ()'s, array/map []'s as well as a dedent after the operand
			int cp;
			suffixes: for(;;) {
				skipWhitespace(beforeToken);
				
				// Check for close brackets / end of file
				if(isCloseParenOrEof(in.peek())) {
					// Current operand is the rightmost operand
					while(!unaryOpStack.isEmpty()) {
						operand = unaryOpStack.pop().makeOp(operand);
					}
					while(! binaryOpStack.isEmpty()) {
						operand = binaryOpStack.pop().makeOp(operand);
					}
					return enrich(operand);
				}

				// Now if we get a de-dent we have to move up the operator stack
				if(beforeToken.getLine() > operand.getFileRange().getEnd().getLine()) {
					int column = in.getCurrentColumnNumber();
					while(!unaryOpStack.isEmpty() 
							&& unaryOpStack.getFirst().getStartColumn() >= column) {
						operand = unaryOpStack.pop().makeOp(operand);
					}
					while(!binaryOpStack.isEmpty() 
							&& binaryOpStack.getFirst().getStartColumn() >= column) {
						operand = binaryOpStack.pop().makeOp(operand);
					}
					
					// If we de-dented back to an exact match on the column of an enclosing
					// expression, insert a virtual semicolon
					if(operand.getStartColumn() == column) {
						binaryOpStack.push(new PartialBinaryOp(BinaryOperator.NEWLINE, operand));
						continue parseExprLoop;
					}
				}
				
				switch(cp = in.read()) {
				case '(': operand = parseFunctionCall(operand); break;
				case '[': operand = parseLookup(operand); break;
				case '.': operand = parseFieldRef(operand); break;
				default: in.unread(); break suffixes;
				}
			}
				
			
			
			
			// Now we are expecting some sort of operator...
			String binaryOp = matchOperator();
			if(binaryOp == null) {
				Expr expr = parseExpr();
				if(expr != null) {
					binaryOp = ";";
					errors.add(new ExpectedOperator(expr));
					in.seek(beforeToken);
				} else {
					throw new SyntaxError("Missing operand after expression", operand.getFileRange());
				}
			}
			
			BinaryOperator binOp = BinaryOperator.fromOp(binaryOp);
			if(binOp == null) {
				errors.add(new UnsupportedBinaryOperator(binaryOp, in.getFileRange(beforeToken)));
				binOp = BinaryOperator.MUL;
			}
			
			// Current operand is the rightmost operand for anything of higher precedence than the
			// operator we just got.
			while(!unaryOpStack.isEmpty()
					&& unaryOpStack.getFirst().getPrecedence().isHigherOrEqual(binOp.getPrecedence())) {
				operand = unaryOpStack.pop().makeOp(operand);
			}
			while(!binaryOpStack.isEmpty() 
					&& binaryOpStack.getFirst().getPrecedence().isHigherOrEqual(binOp.getPrecedence())) {
				operand = binaryOpStack.pop().makeOp(operand);
			}
			
			// Push this operator onto the stack.
			binaryOpStack.push(new PartialBinaryOp(binOp, operand));
		}
	}

	private boolean isListSeparator(BinaryOperator operator) {
		switch(operator) {
		case COMMA:
		case SEMICOLON:
		case NEWLINE:
			return true;
		default:
			return false;
		}
	}

	public boolean isCloseParenOrEof(int cp) {
		return cp == -1 || cp == ')' || cp == ']' || cp == '}';
	}

	private boolean isCloseParen(int codePoint) {
		return ParenType.isCloseParen(codePoint);
	}

	/**
	 * Coming in we have a tree of basically just unary and binary operations and parens and atoms.  
	 * Let's enrich that a bit so we can have ObjectLiteral, ListLiteral, Let, FunctionLiteral,
	 * LetFun.
	 * 
	 * @param node
	 * @return
	 */
	public Expr enrich(Expr node) {
		if(node instanceof UnaryOp) {
			UnaryOp op = (UnaryOp) node;
			final Expr operand = enrich(op.getOperand());
			switch(op.getOperator()) {
			case BULLET: return new ListLiteral(op.getFileRange(), Collections.singletonList(enrich(operand)));
			case LAZY: return new FunctionLiteral(op.getFileRange(), Collections.<FunArg>emptyList(), null, operand);
			default: return op.withNewOperand(enrich(operand));
			}
		} else if(node instanceof BinaryOp) {
			BinaryOp op = (BinaryOp) node;
			// Comma outside of a parentheses should be a list or map without the braces/brackets
			final FileRange range = op.getFileRange();
			switch(op.getOperator()) {
			case COMMA:
			case SEMICOLON:
			case NEWLINE:
				LinkedList<Expr> exprs = new LinkedList<>();
				flattenCommas(op, op.getOperator(), exprs);
				Expr first = exprs.get(0);
				if(isPair(first)) {
					return exprsToObjectLiteral(range, exprs);
				} else if(isListElement(first)) {
					// Bulleted list item - treat as a list
					return exprListToListLiteral(range, exprs, true);
				} else if(isCondCase(first)) {
					return exprListToCond(range, exprs);
				} else {
					// Everything else - treat as a series of steps
					ArrayList<Expr> stepsList = new ArrayList<>(exprs.size());
					for(Expr e : exprs) {
						stepsList.add(enrich(e));
					}
					return new Steps(op.getFileRange(), stepsList);
				}
			case FUNCTION:
				return enrichFunctionLiteral(op);
			case COLON:
				return exprsToObjectLiteral(range, Collections.<Expr>singletonList(op));
			case ASSIGNMENT:
				// Convert to "Let"
				return enrichLet(op);
			case COND:
				return new Cond(op.getFileRange(), Collections.singletonList(new CondCase(op.getFileRange(), op.getLeft(), op.getRight())));
			default:
				return new BinaryOp(op.getOperator(), enrich(op.getLeft()), enrich(op.getRight()));
			}
		} else if(node instanceof Parens) {
			Parens p = (Parens)node;
			Expr e = enrich(p.getExpression());
			switch(p.getParenType()) {
			case BRACES: // Expecting an object
				if(e instanceof ObjectLiteral)
					return e;
				else 
					errors.add(new ExpectedField(e.getFileRange()));
				break;
			case BRACKETS: // Expecting a list
				if(e instanceof ListLiteral) {
					return e;
				} else if(e instanceof Steps) {
					return exprListToListLiteral(e.getFileRange(), ((Steps)e).getSteps(), false);
				} else {
					LinkedList<Expr> exprs = new LinkedList<>();
					flattenCommasOrSemicolons(e, exprs);
					return exprListToListLiteral(e.getFileRange(), exprs, false);
				}
			case PARENS:
				return e; // Eliminate parentheses, they don't mean anything by now
			}
		} else if(node instanceof UnitRef) {
			UnitRef u = (UnitRef) node;
			switch(((UnitRef) node).getParenType()) {
			case BRACES: return new ObjectLiteral(node.getFileRange(), Collections.<String,Field>emptyMap());
			case BRACKETS: return new ListLiteral(u.getFileRange(), Collections.<Expr>emptyList());
			case PARENS: errors.add(new ExpectedExpression(node.getFileRange(), node.toSource())); break;
			}
		}
		return node;
	}

	public Expr enrichLet(BinaryOp op) {
		Expr target = op.getLeft();
		Expr value = op.getRight();
		Expr contract = null;
		String name = null;
		FileRange nameRange = null;
		if(isPair(target)) {
			final BinaryOp targetBOp = (BinaryOp)target;
			target = targetBOp.getLeft();
			contract = targetBOp.getRight();
		}
		if(target instanceof Call) {
			Call call = (Call) target;
			value = makeFunctionLiteral(op.getFileRange(), call.getArguments(), value, contract);
			target = call.getCallee();
			contract = null;
		}
		if(target instanceof IdRef) {
			IdRef id = (IdRef) target;
			name = id.getId();
			nameRange = id.getFileRange();
			// TODO If contract != null, what then ?
			return new Let(nameRange, name, enrich(value));
		} else {
			errors.add(new ExpectedIdentifier(target));
			return op;
		}
	}

	private Expr exprListToCond(FileRange range, LinkedList<Expr> exprs) {
		List<CondCase> cases = new ArrayList<>(exprs.size());
		for(Expr e : exprs) {
			if(!isCondCase(e)) {
				errors.add(new ExpectedCase(e));
				continue;
			}
			BinaryOp caseOp = (BinaryOp)e;
			CondCase c = new CondCase(e.getFileRange(), caseOp.getLeft(), caseOp.getRight());
			cases.add(c);
		}
		return new Cond(range, cases);
	}

	private final boolean isCondCase(Expr e) {
		return (e instanceof BinaryOp) && ((BinaryOp)e).getOperator() == BinaryOperator.COND;
	}

	public Expr exprListToListLiteral(final FileRange range,
			List<Expr> list, boolean requireBullet) {
		ArrayList<Expr> elements = new ArrayList<>();
		if(!list.isEmpty()) {
			for(Expr e : list) {
				Expr eltValue;
				if(isListElement(e)) {
					eltValue = enrich(((UnaryOp)e).getOperand());
				} else {
					if(requireBullet) {
						errors.add(new ExpectedElement(e.getFileRange()));
					}
					eltValue = enrich(e);
				}
				elements.add(eltValue);
			}
		}
		return new ListLiteral(range, elements);
	}

	public Expr exprsToObjectLiteral(FileRange range, Collection<Expr> pairs) {
		// Key/value pair - treat as an object
		LinkedHashMap<String, Field> fields = new LinkedHashMap<>(pairs.size()*2);
		for(Expr e : pairs) {
			if(!isPair(e)) {
				errors.add(new ExpectedField(e.getFileRange()));
				continue;
			}
			final BinaryOp fieldOp = (BinaryOp)e;
			Expr keyExpr = fieldOp.getLeft();
			String key;
			if(keyExpr instanceof IdRef) {
				key = ((IdRef)keyExpr).getId();
			} else if(keyExpr instanceof StringLiteral) {
				key = ((StringLiteral)keyExpr).getString();
			} else {
				errors.add(new ExpectedFieldName("Expected identifier or string; got '"+keyExpr.toSource()+"'", keyExpr.getFileRange()));
				continue;
			}
			Expr valueExpr = enrich(fieldOp.getRight());
			fields.put(key, new Field(keyExpr.getFileRange(), key, valueExpr));
		}
		return new ObjectLiteral(range, fields);
	}

	public Expr enrichFunctionLiteral(BinaryOp op) {
		FileRange range = op.getFileRange();
		// Args should be comma-separated
		List<Expr> exprs = new LinkedList<>();
		Expr argsDef = op.getLeft();
		final Expr body = op.getRight();
		Expr returnContract = null;
		// Optional return type/contract
		if(isPair(argsDef)) {
			returnContract = ((BinaryOp)argsDef).getRight();
			argsDef = ((BinaryOp)argsDef).getLeft();
		}
		// Optional parentheses
		if((argsDef instanceof Parens) && ((Parens)argsDef).getParenType() == ParenType.PARENS) {
			argsDef = ((Parens)argsDef).getExpression();
		}
		if(argsDef instanceof Steps) {
			exprs = ((Steps)argsDef).getSteps();
		} else if(argsDef instanceof UnitRef) {
			// Leave the list empty
		} else {
			flattenCommas(argsDef, BinaryOperator.COMMA, exprs);
		}
		return makeFunctionLiteral(range, exprs, body, returnContract);
	}

	public Expr makeFunctionLiteral(FileRange range, List<Expr> exprs,
			final Expr body, Expr returnContract) {
		List<FunArg> args = exprs.isEmpty() ? Collections.<FunArg>emptyList() : new ArrayList<FunArg>(exprs.size());
		for(Expr argExpr : exprs) {
			String name = null;
			Expr nameExpr = argExpr;
			Expr contract = null;
			if(isPair(nameExpr)) {
				nameExpr = ((BinaryOp) nameExpr).getLeft();
				contract = ((BinaryOp) nameExpr).getRight();
			}
			if(nameExpr instanceof IdRef) {
				name = ((IdRef) nameExpr).getId();
			} else {
				errors.add(new ExpectedIdentifier(nameExpr));
				continue;
			}
			args.add(new FunArg(nameExpr.getFileRange(), name, contract));
		}
		return new FunctionLiteral(range, args, returnContract, body);
	}

	public boolean isListElement(Expr e) {
		return (e instanceof UnaryOp) && ((UnaryOp) e).getOperator() == UnaryOperator.BULLET;
	}

	public boolean isPair(Expr e) {
		return (e instanceof BinaryOp) && ((BinaryOp) e).getOperator() == BinaryOperator.COLON;
	}

	public Expr parseFieldRef(Expr operand) throws IOException {
		FilePos fieldNameStart = in.getFilePos();
		String fieldName = matchID();
		operand = new FieldRef(operand, in.getFileRange(fieldNameStart), fieldName);
		return operand;
	}

	public Expr parseLookup(Expr operand) throws IOException,
			BanjoParseException, ExpectedCloseBracket {
		Expr key = parseExpr();
		if(!in.checkNextChar(']')) {
			throw new ExpectedCloseBracket("Expected ']'.", in.getFilePosAsRange());
		}
		FileRange range = in.getFileRange(operand.getFileRange().getStart());
		operand = new Lookup(range, operand, key);
		return operand;
	}

	public Expr parseFunctionCall(Expr operand) throws IOException,
			BanjoParseException, PrematureEndOfFile, SyntaxError {
		
		LinkedList<Expr> args = new LinkedList<>();
		
		// First check for empty argument list
		skipWhitespace(tokenStartPos);
		if(!in.checkNextChar(')')) {
			// Parse args
			Expr arg = parseExpr();
			if(arg instanceof Steps) {
				args.addAll(((Steps)arg).getSteps());
			} else {
				flattenCommasOrSemicolons(arg, args);
			}
			if(!in.checkNextChar(')')) {
				throw new SyntaxError("Expected ')'", in.getFilePosAsRange());
			}
		}
		FileRange callRange = in.getFileRange(operand.getFileRange().getStart());
		operand = new Call(callRange, operand, args);
		return operand;
	}
	
	private void flattenCommas(Expr arg, BinaryOperator type, List<Expr> exprs) {
		if(arg instanceof BinaryOp) {
			BinaryOp bop = (BinaryOp) arg;
			if(isElementSeparator(bop)) {
				if(bop.getOperator() != type && bop.getOperator() != BinaryOperator.NEWLINE) {
					if(type == BinaryOperator.NEWLINE) type = bop.getOperator();
					else errors.add(new MixedSemicolonAndComma(bop));
				}
				flattenCommas(bop.getLeft(), type, exprs);
				flattenCommas(bop.getRight(), type, exprs);
				return;
			}
		}
		exprs.add(arg);
	}

	private void flattenCommasOrSemicolons(Expr arg, LinkedList<Expr> list) {
		if(arg instanceof BinaryOp) {
			BinaryOp op = (BinaryOp) arg;
			if(isElementSeparator(op)) {
				flattenCommas(op, op.getOperator(), list);
				return;
			}
		}
		list.add(arg);
	}

	public static boolean isElementSeparator(BinaryOp op) {
		return isElementSeparator(op.getOperator());
	}

	public static boolean isElementSeparator(final BinaryOperator operator) {
		return operator == BinaryOperator.COMMA || operator == BinaryOperator.SEMICOLON || operator == BinaryOperator.NEWLINE;
	}
	private Expr parseParentheses() throws BanjoParseException, IOException {
		ParenType parenType = ParenType.forCodePoint(in.read());
		if(parenType == null) {
			in.unread();
			return null;
		}
		int close = parenType.getEndChar();
		Pos startPos = in.getPreviousPosition(new Pos());
		
		skipWhitespace();
		if(in.read() == close) {
			// Empty parentheses means the value is "unit" - the empty tuple,set,or object
			FileRange fileRange = in.getFileRange(startPos);
			return new UnitRef(fileRange, parenType); // For now ...
		} else {
			in.unread();
		}
		Expr expr = parseExpr();
		
		skipWhitespace();
		if(in.read() != close) {
			switch(parenType) {
			case BRACES: throw new ExpectedCloseBrace(in.getFilePosAsRange());
			case BRACKETS: throw new ExpectedCloseBracket(in.getFilePosAsRange());
			case PARENS: throw new ExpectedCloseParen(in.getFilePosAsRange());
			}
		}
		return new Parens(in.getFileRange(startPos), expr, parenType);
	}

	public Pos skipWhitespaceGetPos() throws IOException {
		skipWhitespace();
		return in.getCurrentPosition(new Pos());
	}
	public Collection<BanjoParseException> getErrors() {
		return errors;
	}

	/**
	 * @return true iff we have reached the end of the input
	 */
	public boolean reachedEof() {
		return in.remaining() == 0;
	}
}
