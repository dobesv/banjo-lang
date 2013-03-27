package banjo.parser;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;

import banjo.parser.ast.BinaryOp;
import banjo.parser.ast.BinaryOperator;
import banjo.parser.ast.Call;
import banjo.parser.ast.Cond;
import banjo.parser.ast.CondCase;
import banjo.parser.ast.Ellipsis;
import banjo.parser.ast.Expr;
import banjo.parser.ast.ExprList;
import banjo.parser.ast.Field;
import banjo.parser.ast.FunArg;
import banjo.parser.ast.FunctionLiteral;
import banjo.parser.ast.IdRef;
import banjo.parser.ast.Let;
import banjo.parser.ast.ListLiteral;
import banjo.parser.ast.NumberLiteral;
import banjo.parser.ast.ObjectLiteral;
import banjo.parser.ast.Operator;
import banjo.parser.ast.OperatorRef;
import banjo.parser.ast.ParenType;
import banjo.parser.ast.Precedence;
import banjo.parser.ast.SetLiteral;
import banjo.parser.ast.StringLiteral;
import banjo.parser.ast.StringLiteral.BadStringEscapeSequence;
import banjo.parser.ast.UnaryOp;
import banjo.parser.ast.UnaryOperator;
import banjo.parser.ast.UnitRef;
import banjo.parser.errors.BanjoParseException;
import banjo.parser.errors.ExpectedCase;
import banjo.parser.errors.ExpectedElement;
import banjo.parser.errors.ExpectedExpression;
import banjo.parser.errors.ExpectedField;
import banjo.parser.errors.ExpectedFieldName;
import banjo.parser.errors.ExpectedIdentifier;
import banjo.parser.errors.ExpectedOperator;
import banjo.parser.errors.IncorrectIndentation;
import banjo.parser.errors.MissingDigitsAfterDecimalPoint;
import banjo.parser.errors.MissingValueForTableColumn;
import banjo.parser.errors.MixedSemicolonAndComma;
import banjo.parser.errors.PrematureEndOfFile;
import banjo.parser.errors.SyntaxError;
import banjo.parser.errors.UnexpectedCloseParen;
import banjo.parser.errors.UnexpectedContract;
import banjo.parser.errors.UnexpectedDecimalPoint;
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

	public static boolean isIdentifierStart(int cp) {
		return cp == '_' || cp == '$' || cp == '\\' || Character.isLetter(cp);
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
	 * @return
	 * @throws IOException
	 */
	public String matchID() throws IOException {
		final int first = in.read();
		if(!isIdentifierStart(first)) {
			in.unread();
			return null;
		}
		boolean escape = first == '\\';
		buf.setLength(0); // Reset buffer
		buf.appendCodePoint(first);
		for(;;) {
			int cp = in.read();
			if(cp == '\\') {
				escape = true;
			} else {
				if(cp == -1 || !(escape || isIdentifierPart(cp))) {
					in.unread();
					return buf.toString();
				}
				buf.appendCodePoint(cp);
			}
		}
	}
	
	public static final boolean isOperatorChar(int codePoint) {
		switch(codePoint) {
		case '"': 
		case '\'':
		case '_':
		case '.':
		case '\\':
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
		if(!isOperatorChar(first)) {
			in.unread();
			return null;
		}
		switch(first) {
			case ',': return ",";
			case ';': return ";";
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
	
	public StringLiteral parseStringLiteral() throws IOException {
		in.getCurrentPosition(tokenStartPos);
		
		int cp = in.read();
		if(cp != '"' && cp != '\'') {
			in.seek(tokenStartPos);
			return null;
		}
		int quoteType = cp;
			
		
	    buf.setLength(0);
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
	                getErrors().add(new BadStringEscapeSequence("Unknown escape sequence "+codePointToString(cp), in.getFileRange(afterBackslash)));
	                break;
	            }
	        }
	    }
	    if(cp == -1) getErrors().add(new PrematureEndOfFile("End of file in string literal", in.getFileRange(tokenStartPos)));
		return new StringLiteral(in.getFileRange(tokenStartPos), buf.toString());
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
		final int digitsRightOfDecimalPoint = digits - digitsLeftOfDecimalPoint;
		if(!isInteger && digitsRightOfDecimalPoint == 0) {
			if(!isNumber) {
				in.seek(tokenStartPos);
				return null;
			} else {
				getErrors().add(new MissingDigitsAfterDecimalPoint("Missing digits after decimal point", in.getFileRange(tokenStartPos)));
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
	 */
	private IdRef parseIdRef() throws IOException {
		in.getCurrentPosition(tokenStartPos);
		String identifier = matchID();
		if(identifier == null)
			return null;
		FileRange identRange = in.getFileRange(tokenStartPos);
		return new IdRef(identRange, identifier);
	}

	/**
	 * Parse an operator.  If the current parse position has an operator,
	 * this consumes it and returns it.  Otherwise, this resets the parse
	 * position and returns null.
	 * 
	 * @return An OperatorRef if the parse is successful; null otherwise
	 */
	private OperatorRef parseOperatorRef() throws IOException {
		in.getCurrentPosition(tokenStartPos);
		String operator = matchOperator();
		if(operator == null)
			return null;
		FileRange opRange = in.getFileRange(tokenStartPos);
		return new OperatorRef(opRange, operator);
	}
	
	/**
	 * Check for an Ellipsis at the current parse position.
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

	abstract class PartialOp {
		abstract Expr makeOp(Expr operand) throws BanjoParseException;
		abstract int getStartColumn();
		abstract Operator getOperator();
		ParenType getParenType() { return getOperator().getParenType(); }
		Precedence getPrecedence() { return getOperator().getPrecedence(); }
		public boolean isOpenParen(ParenType closeParenType) {
			return closeParenType == getParenType();
		}
	}
	class PartialBinaryOp extends PartialOp {
		private final BinaryOperator operator;
		private final Expr operand;
		private final FileRange opRange;
		public PartialBinaryOp(BinaryOperator operator, FileRange opRange, Expr operand) {
			super();
			this.operator = operator;
			this.operand = operand;
			this.opRange = opRange;
		}
		public Expr makeOp(Expr secondOperand) throws ExpectedExpression {
			if(secondOperand == null) {
				if(operator == BinaryOperator.CALL) {
					return new UnaryOp(in.getFileRange(opRange.getStart()), UnaryOperator.CALL, operand);
				} else {
					throw new ExpectedExpression(in.getFileRange(opRange.getEnd()));
				}
			}
			// The second operand must be indented to at least the same position as the first
			if(secondOperand.getStartColumn() < getStartColumn()) {
				errors.add(new IncorrectIndentation(secondOperand.getFileRange(), getStartColumn(), true));
			}
			return new BinaryOp(operator, getOperand(), secondOperand);
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
		public FileRange getOpRange() {
			return opRange;
		}
	}
	
	class PartialUnaryOp extends PartialOp {
		final UnaryOperator operator;
		final FileRange opRange;
		public PartialUnaryOp(UnaryOperator operator, FileRange opRange) {
			super();
			this.operator = operator;
			this.opRange = opRange;
		}
		public Expr makeOp(Expr operand) throws BanjoParseException {
			if(operand == null) {
				if(operator.isParen()) {
					return new UnitRef(in.getFileRange(opRange.getStart()), operator.getParenType());
				} else {
					throw new ExpectedExpression(in.getFileRange(opRange.getStart()));
				}
			}
			if(operand.getStartColumn() < getStartColumn()) {
				// Operand should be at the same level or higher indentation as the unary operator itself
				errors.add(new IncorrectIndentation(operand.getFileRange(), getStartColumn(), true));
			}
			return new UnaryOp(new FileRange(opRange,operand.getFileRange()), operator, operand);
		}
		public int getStartColumn() {
			return opRange.getStart().getColumn();
		}
		public UnaryOperator getOperator() {
			return operator;
		}
		@Override
		public String toString() {
			return "("+operator.getOp()+" _)";
		}
	}
	
	public Expr parseExpr() throws IOException, BanjoParseException {
		return enrich(parseExpr(null));
	}
	
	/**
	 * Parse input until we match the given parentheses or reach EOF.  
	 * 
	 * If inParen is null,
	 * EOF is considered success; otherwise, EOF is an error.
	 * 
	 * @param inParen If set, assume we are inside that kind of parentheses
	 */
	
	private Expr parseExpr(ParenType inParen) throws IOException, BanjoParseException {
		LinkedList<PartialOp> opStack = new LinkedList<>();
		Pos beforeToken = new Pos();
		Expr operand = null;
		for(;;) {
			skipWhitespace(beforeToken);
			
			// Now if we get a de-dent we have to move up the operator stack
			if(operand != null && beforeToken.getLine() > operand.getFileRange().getEnd().getLine()) {
				int column = in.getCurrentColumnNumber();
				while(!opStack.isEmpty() 
						&& opStack.getFirst().getStartColumn() >= column) {
					operand = opStack.pop().makeOp(operand);
				}
				
				// If we de-dented back to an exact match on the column of an enclosing
				// expression, insert a newline operator
				if(operand.getStartColumn() == column) {
					opStack.push(new PartialBinaryOp(BinaryOperator.NEWLINE, in.getFilePosAsRange(), operand));
					operand = null;
				}
			}
			
			Expr token = parseAtom();
			
			if(token != null) {
				operand = parseExprToken(opStack, operand, token);
			} else {
				// Not an atom, binary operator, or unary operator
				// Should be a paren, brace, bracket, or EOF
				int cp = in.read();
				ParenType closeParenType = cp < Character.MAX_VALUE ? ParenType.forCloseChar((char)cp) : null;
				if(closeParenType != null || cp == -1) {
					// If there is a trailing comma, semicolon, or newline we can pop it off the stack
					if(operand == null &&
					   !opStack.isEmpty() && 
				       (opStack.getFirst() instanceof PartialBinaryOp) &&
					   isListSeparator(((PartialBinaryOp) opStack.getFirst()).getOperator())) {
						operand = ((PartialBinaryOp) opStack.pop()).getOperand();
					}
					
					// Now pop 
					boolean matchedOpen = false;
					while(!opStack.isEmpty()) {
						PartialOp po = opStack.pop();
						operand = po.makeOp(operand);
						if(po.isOpenParen(closeParenType)) {
							matchedOpen = true;
							break;
						} else if(po.getParenType() != null) {
							errors.add(new UnexpectedCloseParen(in.getFileRange(beforeToken), closeParenType));
						}
					}
					if(!matchedOpen) {
						if(closeParenType == inParen) {
							return operand;
						} else if(cp == -1 && inParen != null) {
							throw new PrematureEndOfFile("Found EOF, was expecting '"+(char)inParen.getEndChar()+"'", in.getFilePosAsRange());
						} else if(errors.size() > 0) {
							throw errors.removeLast();
						} else {
							throw new ExpectedExpression(in.getFilePosAsRange());
						}
					}
				} else {
					ParenType parenType = cp < Character.MAX_VALUE ? ParenType.forChar((char)cp) : null;
					if(parenType != null) {
						if(operand == null) {
							UnaryOperator operator = UnaryOperator.fromParenType(parenType);
							if(operator == null) {
								// Currently all parens are valid on their own, so this shouldn't happen really
								throw new SyntaxError(in.getFileRange(tokenStartPos));
							}
							opStack.push(new PartialUnaryOp(operator, in.getFileRange(tokenStartPos)));
						} else {
							// Call/lookup
							BinaryOperator operator = BinaryOperator.fromParenType(parenType);
							if(operator == null) {
								final UnaryOperator unaryParenType = UnaryOperator.fromParenType(parenType);
								if(unaryParenType == null) {
									throw new SyntaxError(in.getFilePosAsRange());
								}
								
								// Insert "missing" operator and continue
								final FileRange betweenRange = between(operand, token);
								errors.add(new ExpectedOperator(betweenRange));
								pushPartialBinaryOp(BinaryOperator.MISSING, betweenRange, operand, opStack);
								opStack.push(new PartialUnaryOp(unaryParenType, in.getFileRange(tokenStartPos)));
							} else {
								pushPartialBinaryOp(operator, in.getFileRange(tokenStartPos), operand, opStack);
							}
							operand = null;
						}
					} else {
						throw new SyntaxError(in.getFilePosAsRange());
					}
				}
			}
		}
	}

	private Expr parseExprToken(LinkedList<PartialOp> opStack, Expr operand,
			Expr token) throws BanjoParseException {
		if(token instanceof OperatorRef) {
			final OperatorRef opRef = (OperatorRef)token;
			if(operand != null) {
				// Infix position
				BinaryOperator operator = BinaryOperator.fromOp(opRef.getOp());
				if(operator == null) {
					errors.add(new UnsupportedBinaryOperator(opRef.getOp(), opRef.getFileRange()));
					operator = BinaryOperator.INVALID;
				}
				pushPartialBinaryOp(operator, opRef.getFileRange(), operand, opStack);
			} else {
				// Prefix position
				UnaryOperator operator = UnaryOperator.fromOp(opRef.getOp());
				if(operator == null) {
					errors.add(new UnsupportedUnaryOperator(opRef.getOp(), opRef.getFileRange()));
					operator = UnaryOperator.INVALID;
				}
				opStack.push(new PartialUnaryOp(operator, opRef.getFileRange()));
			}
			return null;
		} else {
			if(operand != null) {
				errors.add(new ExpectedOperator(between(operand, token)));
			}
			return token;
		}
	}

	private void pushPartialBinaryOp(BinaryOperator operator, FileRange range, Expr operand, LinkedList<PartialOp> opStack) throws BanjoParseException {
		// Note: should be a binary op here ...
		// Current operand is the rightmost operand for anything of higher precedence than the
		// operator we just got.
		Precedence prec = operator.getPrecedence();
		while(!opStack.isEmpty() 
				&& opStack.getFirst().getOperator().isParen() == false
				&& opStack.getFirst().getPrecedence().isHigherOrEqual(prec)) {
			operand = opStack.pop().makeOp(operand);
		}
		
		opStack.push(new PartialBinaryOp(operator, range, operand));
	}

	private FileRange between(Expr a, Expr b) {
		return new FileRange(a.getFileRange().getFilename(), a.getFileRange().getEnd(), b.getFileRange().getStart());
	}

	public String codePointToString(int cp) {
		return new String(Character.toChars(cp));
	}

	private Expr parseAtom() throws IOException {
		Expr operand = null;
		if((operand = parseIdRef()) == null &&
			(operand = parseOperatorRef()) == null &&
			(operand = parseStringLiteral()) == null &&
			(operand = parseNumberLiteral()) == null && 
			(operand = parseEllipsis()) == null) {
			return null;
		}
		return operand;
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

	/**
	 * Coming in we have a tree of basically just unary and binary operations and parens and atoms.  
	 * Let's enrich that a bit so we can have ObjectLiteral, ListLiteral, Let, FunctionLiteral,
	 * LetFun.
	 * 
	 * @param node
	 * @return
	 */
	private Expr enrich(Expr node) {
		if(node instanceof UnaryOp) {
			UnaryOp op = (UnaryOp) node;
			final Expr operand = enrich(op.getOperand());
			switch(op.getOperator()) {
			case LIST_ELEMENT: return new ListLiteral(op.getFileRange(), Collections.singletonList(enrich(operand)));
			case SET_ELEMENT: return new SetLiteral(op.getFileRange(), Collections.singletonList(enrich(operand)));
			case LAZY: return new FunctionLiteral(op.getFileRange(), Collections.<FunArg>emptyList(), null, operand);
			case KEYWORD: return exprsToObjectLiteral(op.getFileRange(), Collections.singletonList(operand));
			case PARENS: return operand;
			case LIST_LITERAL:
				if(operand instanceof ListLiteral) {
					return operand;
				} else if(operand instanceof ExprList) {
					return exprListToListLiteral(operand.getFileRange(), ((ExprList)operand).getSteps(), false);
				} else {
					LinkedList<Expr> exprs = new LinkedList<>();
					flattenCommasOrSemicolons(operand, exprs);
					return exprListToListLiteral(operand.getFileRange(), exprs, false);
				}
			case OBJECT_OR_SET_LITERAL: // Expecting an object or set
				if(operand instanceof ObjectLiteral)
					return operand;
				else if(operand instanceof ExprList)
					return new SetLiteral(node.getFileRange(), ((ExprList)operand).getSteps());
				else 
					errors.add(new ExpectedField(operand.getFileRange()));
				break;
			case CALL:
				return new Call(op.getFileRange(), operand, Collections.<Expr>emptyList());
			default: return op.withNewOperand(operand);
			}
		} else if(node instanceof BinaryOp) {
			BinaryOp op = (BinaryOp) node;
			// Comma outside of a parentheses should be a list or map without the braces/brackets
			final FileRange range = op.getFileRange();
			switch(op.getOperator()) {
			case COMMA:
			case SEMICOLON:
			case NEWLINE: {
				LinkedList<Expr> exprs = new LinkedList<>();
				flattenCommas(op, op.getOperator(), exprs);
				Expr first = exprs.get(0);
				if(isPair(first) || isKeyword(first) || isTableHeader(first)) {
					return exprsToObjectLiteral(range, exprs);
				} else if(isListElement(first)) {
					// Bulleted list item - treat as a list
					return exprListToListLiteral(range, exprs, true);
				} else if(isSetElement(first)) {
					// Bulleted set item - treat as a list
					return exprListToSetLiteral(range, exprs, true);
				} else if(isCondCase(first)) {
					return exprListToCond(range, exprs);
				} else {
					// Everything else - treat as a series of steps
					ArrayList<Expr> exprList = new ArrayList<>(exprs.size());
					for(Expr e : exprs) {
						exprList.add(enrich(e));
					}
					return new ExprList(op.getFileRange(), exprList);
				}
			}
			case CALL: {
				List<Expr> args = new ArrayList<Expr>();
				// TODO Can't distinguish between a(()) and a() in this system...
				List<Expr> exprs = flattenCommasOrSemicolons(op.getRight(), new LinkedList<Expr>());
				for(Expr expr : exprs) {
					args.add(enrich(expr));
				}
				return new Call(op.getFileRange(), enrich(op.getLeft()), args);
			}
			case FUNCTION:
				return enrichFunctionLiteral(op);
			case PAIR:
			case TABLE_PAIR:
				return exprsToObjectLiteral(range, Collections.<Expr>singletonList(op));
			case ASSIGNMENT:
				// Convert to "Let"
				return enrichLet(op);
			case COND:
				return new Cond(op.getFileRange(), Collections.singletonList(new CondCase(op.getFileRange(), op.getLeft(), op.getRight())));
			default:
				return new BinaryOp(op.getOperator(), enrich(op.getLeft()), enrich(op.getRight()));
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

	private Expr enrichLet(BinaryOp op) {
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
		if(isCallWithArgs(target)) {
			BinaryOp call = (BinaryOp) target;
			value = enrichFunctionLiteral(op.getFileRange(), call.getRight(), contract, value);
			target = call.getLeft();
			contract = null;
		} else if(target instanceof UnaryOp && ((UnaryOp) target).getOperator() == UnaryOperator.CALL) {
			UnaryOp call = (UnaryOp) target;
			value = new FunctionLiteral(op.getFileRange(), Collections.<FunArg>emptyList(), contract, value);
			target = call.getOperand();
			contract = null;
		}
		if(target instanceof IdRef) {
			IdRef id = (IdRef) target;
			name = id.getId();
			nameRange = id.getFileRange();
			if(contract != null) {
				errors.add(new UnexpectedContract(contract));
			}
			return new Let(nameRange, name, enrich(value));
		} else {
			errors.add(new ExpectedIdentifier(target));
			return op;
		}
	}

	private boolean isCallWithArgs(Expr target) {
		return target instanceof BinaryOp && ((BinaryOp)target).getOperator() == BinaryOperator.CALL;
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

	private Expr exprListToListLiteral(final FileRange range,
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

	private Expr exprListToSetLiteral(final FileRange range,
			List<Expr> list, boolean requireBullet) {
		ArrayList<Expr> elements = new ArrayList<>();
		if(!list.isEmpty()) {
			for(Expr e : list) {
				Expr eltValue;
				if(isSetElement(e)) {
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
		return new SetLiteral(range, elements);
	}

	private Expr exprsToObjectLiteral(FileRange range, Collection<Expr> pairs) {
		// Key/value pair - treat as an object
		LinkedHashMap<String, Field> fields = new LinkedHashMap<>(pairs.size()*2);
		List<Expr> headings = null;
		for(Expr e : pairs) {
			Expr keyExpr;
			Expr valueExpr;
			if(isPair(e)) {
				final BinaryOp fieldOp = (BinaryOp)e;
				keyExpr = fieldOp.getLeft();
				valueExpr = fieldOp.getRight();
			} else if(isKeyword(e)) {
				final UnaryOp fieldOp = (UnaryOp)e;
				keyExpr = fieldOp.getOperand();
				// TODO Come up with a better opaque unique object system
				valueExpr = new IdRef(keyExpr.getFileRange(), "$unique");
			} else if(isTableHeader(e)) {
				final UnaryOp headerOp = (UnaryOp)e;
				headings = flattenCommasOrSemicolons(headerOp.getOperand(), new ArrayList<Expr>());
				continue;
			} else {
				errors.add(new ExpectedField(e.getFileRange()));
				continue;
			}
			Expr contract = null;
			if(isPair(keyExpr)) {
				final BinaryOp targetBOp = (BinaryOp)keyExpr;
				keyExpr = targetBOp.getLeft();
				contract = targetBOp.getRight();
			}
			if(isCallWithArgs(keyExpr)) {
				BinaryOp call = (BinaryOp) keyExpr;
				valueExpr = makeFunctionLiteral(e.getFileRange(), flattenCommasOrSemicolons(call.getRight(), new ArrayList<Expr>()), valueExpr, contract);
				keyExpr = call.getLeft();
				contract = null;
			}
			if(contract != null) {
				errors.add(new UnexpectedContract(contract));
			}
			String key;
			if(keyExpr instanceof IdRef) {
				key = ((IdRef)keyExpr).getId();
			} else if(keyExpr instanceof StringLiteral) {
				key = ((StringLiteral)keyExpr).getString();
			} else {
				errors.add(new ExpectedFieldName("Expected identifier or string; got "+keyExpr.getClass().getSimpleName()+" '"+keyExpr.toSource()+"'", keyExpr.getFileRange()));
				continue;
			}
			// If this is a table, construct the row
			if(headings != null) {
				List<Expr> values = flattenCommasOrSemicolons(valueExpr, new ArrayList<Expr>(headings.size()));
				for(int i=headings.size(); i < values.size(); i++) {
					final Expr val = values.get(i);
					errors.add(new ExtraTableColumn(i, headings.size(), val.toSource(), val.getFileRange()));
				}
				if(values.size() < headings.size()) {
					errors.add(new MissingValueForTableColumn(values.size(), headings.size(), valueExpr.getFileRange(), headings.get(values.size()).toSource()));
					continue;
				}
				ArrayList<Expr> fieldOps = new ArrayList<>(headings.size());
				for(int i=0; i < headings.size(); i++) {
					fieldOps.add(new BinaryOp(BinaryOperator.PAIR, headings.get(i), values.get(i)));
				}
				valueExpr = exprsToObjectLiteral(valueExpr.getFileRange(), fieldOps);
			}
			fields.put(key, new Field(keyExpr.getFileRange(), key, enrich(valueExpr)));
		}
		return new ObjectLiteral(range, fields);
	}

	private Expr enrichFunctionLiteral(BinaryOp op) {
		Expr argsDef = op.getLeft();
		final Expr body = op.getRight();
		FileRange range = op.getFileRange();
		return enrichFunctionLiteral(argsDef, body, range);
	}

	private Expr enrichFunctionLiteral(Expr argsDef, final Expr body,
			FileRange range) {
		Expr returnContract = null;
		// Optional return type/contract
		if(isPair(argsDef)) {
			returnContract = ((BinaryOp)argsDef).getRight();
			argsDef = ((BinaryOp)argsDef).getLeft();
		}
		return enrichFunctionLiteral(range, argsDef, returnContract, body);
	}

	private Expr enrichFunctionLiteral(FileRange range, Expr args,
			Expr contract, final Expr body) {
		// Args should be comma-separated
		List<Expr> exprs = new LinkedList<>();
		// Optional parentheses
		if((args instanceof UnaryOp) && ((UnaryOp)args).getOperator().getParenType() == ParenType.PARENS) {
			args = ((UnaryOp)args).getOperand();
		}
		if(args instanceof ExprList) {
			exprs = ((ExprList)args).getSteps();
		} else if(args instanceof UnitRef) {
			// Leave the list empty
		} else {
			flattenCommas(args, BinaryOperator.COMMA, exprs);
		}
		return makeFunctionLiteral(range, exprs, body, contract);
	}

	private Expr makeFunctionLiteral(FileRange range, List<Expr> exprs,
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

	private boolean isListElement(Expr e) {
		return isUnaryOp(e, UnaryOperator.LIST_ELEMENT);
	}
	private boolean isSetElement(Expr e) {
		return isUnaryOp(e, UnaryOperator.SET_ELEMENT);
	}

	private boolean isPair(Expr e) {
		return (e instanceof BinaryOp) && 
				(((BinaryOp) e).getOperator() == BinaryOperator.PAIR ||
				 ((BinaryOp) e).getOperator() == BinaryOperator.TABLE_PAIR);
	}
	private boolean isKeyword(Expr e) {
		return isUnaryOp(e, UnaryOperator.KEYWORD);
	}
	private boolean isTableHeader(Expr e) {
		return isUnaryOp(e, UnaryOperator.TABLE_HEADER);
	}

	private boolean isUnaryOp(Expr e, final UnaryOperator operator) {
		return (e instanceof UnaryOp) && ((UnaryOp) e).getOperator() == operator;
	}

	private void flattenCommas(Expr arg, BinaryOperator type, List<Expr> exprs) {
		if(arg instanceof BinaryOp) {
			BinaryOp bop = (BinaryOp) arg;
			if(isElementSeparator(bop)) {
				final BinaryOperator boper = bop.getOperator();
				if(boper != type && bop.getOperator() != BinaryOperator.NEWLINE) {
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

	private List<Expr> flattenCommasOrSemicolons(Expr arg, List<Expr> list) {
		if(arg instanceof BinaryOp) {
			BinaryOp op = (BinaryOp) arg;
			if(isElementSeparator(op)) {
				flattenCommas(op, op.getOperator(), list);
				return list;
			}
		}
		list.add(arg);
		return list;
	}

	private static boolean isElementSeparator(BinaryOp op) {
		return isElementSeparator(op.getOperator());
	}

	private static boolean isElementSeparator(final BinaryOperator operator) {
		return operator == BinaryOperator.COMMA || operator == BinaryOperator.SEMICOLON || operator == BinaryOperator.NEWLINE;
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
