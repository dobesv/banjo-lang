package banjo.expr.source;

import java.io.IOException;

import banjo.expr.ParenType;
import banjo.expr.source.BadSourceExpr.MissingCloseParen;
import banjo.expr.source.Operator.Position;
import banjo.expr.token.BadIdentifier;
import banjo.expr.token.Identifier;
import banjo.expr.token.NumberLiteral;
import banjo.expr.token.OperatorRef;
import banjo.expr.token.StringLiteral;
import banjo.expr.token.TokenScanner;
import banjo.expr.token.TokenVisitor;
import banjo.expr.util.FileRange;
import banjo.expr.util.ParserReader;
import banjo.expr.util.SourceFileRange;
import fj.data.List;

/**
 * Change input into an AST.
 */
public class SourceExprFactory implements TokenVisitor<SourceExprFactory> {
	private static final String DEFAULT_ID_PREFIX = "/temp/";
	public final String sourceFile;
	public final List<PartialOp> opStack;
	public final SourceExpr operand;
	public final SourceExpr result;
	public final int operandIndentColumn;

	abstract class PartialOp {
		protected final Operator operator;
		protected final List<SourceFileRange> ranges;
		protected final SourceExpr operatorExpr;
		protected final int indentColumn;

		/**
		 *
		 */
		public PartialOp(List<SourceFileRange> ranges, Operator operator, SourceExpr operatorExpr, int indentColumn) {
			this.ranges = ranges.append(operatorExpr.getSourceFileRanges()).sort(SourceFileRange.ORD);
			this.operator = operator;
			this.operatorExpr = operatorExpr;
			this.indentColumn = indentColumn;
		}

		/**
		 * Create a node for a non-parentheses operator with a right-hand expression.
		 */
		public SourceExpr rhs(SourceExpr rightOperand, int rightOperandIndentColumn) {
			final List<SourceFileRange> ranges = SourceFileRange.append(this.ranges, rightOperand.getSourceFileRanges());
			return _rhs(rightOperand, ranges, rightOperandIndentColumn, this.operatorExpr.getSourceFileRanges());
		}

		/**
		 * Create a node for a parenthesized expression.
		 */
		public SourceExpr closeParen(SourceExpr rightOperand, FileRange closeParenRange, int rightOperandIndentColumn) {
			final List<SourceFileRange> operatorRanges = SourceFileRange.snoc(this.operatorExpr.getSourceFileRanges(), sfr(closeParenRange));
			final List<SourceFileRange> ranges = this.ranges.snoc(sfr(closeParenRange));
			return _rhs(rightOperand, ranges, rightOperandIndentColumn, operatorRanges);
		}

		private SourceExpr _rhs(SourceExpr rightOperand, final List<SourceFileRange> ranges, int rightOperandIndentColumn, final List<SourceFileRange> operatorRanges) {
			if(this.operator == Operator.INVALID) {
				return new BadSourceExpr.UnsupportedOperator(ranges, this.operatorExpr.toSource());
			}
			if(badIndentation(rightOperandIndentColumn)) {
				rightOperand = new BadSourceExpr.IncorrectIndentation(
						rightOperand.getSourceFileRanges(),
						rightOperandIndentColumn,
						this.indentColumn
				);
			}
			return makeOp(rightOperand, ranges, operatorRanges);
		}

		public boolean badIndentation(int rightOperandIndentColumn) {
			//return false;
			return this.indentColumn > 0 && rightOperandIndentColumn < this.indentColumn;
		}

		/**
		 * Create a node for a non-parentheses operator with a right-hand expression.
		 *
		 * @param sourceExpr Right-hand operand for a binary op, or the operand for a unary op
		 * @param ranges Full source file ranges for the expression, including operators
		 * @param operatorRanges Source file ranges for the operator; a single entry for most operators,
		 *     but two for a parenthesis or ternary operator.
		 */
		abstract SourceExpr makeOp(SourceExpr sourceExpr, List<SourceFileRange> ranges, List<SourceFileRange> operatorRanges);

		public Operator getOperator() {
			return this.operator;
		}
		ParenType getParenType() { return getOperator().getParenType(); }
		Precedence getPrecedence() { return getOperator().getPrecedence(); }
		public boolean isOpenParen(ParenType closeParenType) {
			return isParen() && closeParenType == getParenType();
		}
		public boolean isParen() {
			return getOperator().isParen();
		}
	}
	class PartialBinaryOp extends PartialOp {
		private final SourceExpr leftOperand;

		/**
		 * Create a partial binary op.  We have the left-hand operand and the the operator but we're waiting for the right-hand
		 * operand.
		 */
		public PartialBinaryOp(Operator operator, SourceExpr operand, SourceExpr operatorExpr, int indentColumn) {
			super(operand.getSourceFileRanges().append(operatorExpr.getSourceFileRanges()),
					operator, operatorExpr, indentColumn);
			this.leftOperand = operand;
		}

		public PartialBinaryOp(Operator operator, SourceExpr operand, SourceFileRange operatorRange, int indentColumn) {
			this(operator, operand, new OperatorRef(operatorRange, indentColumn, operator.getOp()), indentColumn);
		}

		@Override
		public SourceExpr makeOp(SourceExpr rightOperand, List<SourceFileRange> ranges, List<SourceFileRange> operatorRanges) {
			return new BinaryOp(ranges, this.operator, operatorRanges, this.leftOperand, rightOperand);
		}

		@Override
		public String toString() {
			if(this.operator.isParen())
				return getOperand().toSource()+this.operator.getParenType().getStartChar() +"_"+this.operator.getParenType().getEndChar();
			return "(" + getOperand().toSource()+" "+this.operator.getOp() +" _)";
		}
		public SourceExpr getOperand() {
			return this.leftOperand;
		}

		public List<SourceFileRange> getSourceFileRanges() {
			return this.ranges;
		}
	}

	class PartialUnaryOp extends PartialOp {

		public PartialUnaryOp(List<SourceFileRange> ranges, Operator operator, int indentColumn) {
			super(ranges, operator, new OperatorRef(ranges, indentColumn, operator.getOp()), indentColumn);
		}

		public PartialUnaryOp(SourceFileRange range, Operator operator, int indentColumn) {
			this(List.single(range), operator, indentColumn);
		}

		@Override
		SourceExpr makeOp(SourceExpr rightOperand, List<SourceFileRange> ranges, List<SourceFileRange> operatorRanges) {
			if(this.operator == Operator.NEGATE && rightOperand instanceof NumberLiteral) {
				return ((NumberLiteral)rightOperand).negate(operatorRanges.append(ranges));
			} else if(this.operator == Operator.PLUS && rightOperand instanceof NumberLiteral) {
				return rightOperand;
			}

			return new UnaryOp(ranges, this.operator, operatorRanges, rightOperand);
		}

		@Override
		public String toString() {
			if(this.operator.isParen())
				return this.operator.getParenType().getStartChar() +"_"+this.operator.getParenType().getEndChar();
			return "("+this.operator.getOp()+" _)";
		}
	}

	public SourceExprFactory(String sourceFile, List<PartialOp> opStack, SourceExpr operand, SourceExpr result, int operandIndentColumn) {
		super();
		this.sourceFile = sourceFile;
		this.opStack = opStack;
		this.operand = operand;
		this.result = result;
		this.operandIndentColumn = operandIndentColumn;
	}

	/**
	 * Construct a parser with the given string as the source file name to use when
	 * reporting errors.
	 */
	public SourceExprFactory(String sourceFile) {
		this(sourceFile, List.nil(), null, null, 0);
	}

	/**
	 * Constructor that doesn't take a filename, for when you don't care about filenames
	 */
	public SourceExprFactory() {
		this("");
	}

	/**
	 * Parse the input fully.  The final return value is the result of visitEof().  It may be null
	 * if there were syntax errors in the source file, or the file was empty;
	 *
	 * @param in Input source to read; will be closed by this function
	 * @return Root of the parsed syntax tree
	 * @throws IOException If the underlying stream throws IOException
	 */
	public SourceExpr parse(ParserReader in) throws IOException {
		try {
			final TokenScanner scanner = new TokenScanner();
			SourceExprFactory parseResult = scanner.scan(in, this);
			SourceExpr operand = parseResult.getOperand();
			if(operand == null)
				return new EmptyExpr();
			return operand;
		} finally {
			in.close();
		}
	}

	public SourceExpr parse(String source) throws IOException {
		final TokenScanner scanner = new TokenScanner();
		SourceExprFactory parseResult = scanner.scan(source, this);
		SourceExpr operand = parseResult.getOperand();
		if(operand == null)
			return new EmptyExpr();
		return operand;
	}

	/**
	 * Helper for checkIndentDedent to decide whether an expression is closed
	 * off by a newline and possible dedent.
	 */
	boolean shouldPopBasedOnDedent(int column) {
		if(this.opStack.isEmpty()) return false;
		PartialOp first = this.opStack.head();
		final int indentColumn = first.indentColumn;
		return !first.isParen() && first.getOperator() != Operator.NEWLINE;
		//return column < indentColumn || (!first.isParen() && column == indentColumn);
	}

	SourceExprFactory popPartialOp() {
		final SourceExpr operand = this.operand;
		if(operand == null) throw new NullPointerException();
		final PartialOp op = opStack.head();
		return update(opStack.tail(), op.rhs(operand, operandIndentColumn), op.indentColumn);
	}

	SourceExprFactory pushNewline(FileRange range) {
		return applyOperatorPrecedenceToStack(Operator.NEWLINE)
				.pushPartialBinaryOp(Operator.NEWLINE, FileRange.between(operand.getSourceFileRanges().last().getFileRange(), range), "\n");
	}

	/**
	 * Check for indent/dedent prior to the given token.  May consume the contents of nodes with the assumption it is
	 * whitespace/comments only.
	 *
	 * @param range The range of the token
	 * @param indentColumn TODO
	 */
	private SourceExprFactory checkNewline(FileRange range, int indentColumn) {
		final int line = range.getStartLine();
		SourceExpr operand = this.getOperand();

		// Check if we have an operand, it has source location, and we have moved to the next line
		if(operand == null || operand.getSourceFileRanges().isEmpty() || line <= operand.getSourceFileRanges().last().getEndLine())
			return this;

		// Now if we get a de-dent we have to move up the operator stack
		SourceExprFactory ps = this;
		while(ps.shouldPopBasedOnDedent(indentColumn)) {
			ps = ps.popPartialOp();
		}

		// Insert a newline operation since we moved to a new line
		return ps.pushNewline(range);
	}

	private SourceExprFactory update(List<PartialOp> opStack, SourceExpr operand, SourceExpr result, int operandIndentColumn) {
		return new SourceExprFactory(sourceFile, opStack, operand, result, operandIndentColumn);
	}
	private SourceExprFactory update(List<PartialOp> opStack, SourceExpr operand, int operandIndentColumn) {
		return update(opStack, operand, result, operandIndentColumn);
	}

	public SourceExprFactory pushPartialOp(PartialOp partialOp) {
		return update(opStack.cons(partialOp), null, partialOp.indentColumn);
	}

	private SourceExprFactory visitAtom(FileRange range, int indentColumn, SourceExpr token) {
		return checkNewline(range, indentColumn)
				.maybeJuxtaposition(range)
				.withOperand(token, indentColumn);
	}

	private SourceExprFactory maybeJuxtaposition(FileRange nextTokenRange) {
		if(operand != null) {
			return juxtaposition(nextTokenRange);
		}
		return this;
	}

	private SourceExprFactory juxtaposition(FileRange nextTokenRange) {
		final FileRange betweenRange = FileRange.between(operand.getSourceFileRanges().last().getFileRange(), nextTokenRange);
		return applyOperatorPrecedenceToStack(Operator.JUXTAPOSITION)
				.pushPartialBinaryOp(Operator.JUXTAPOSITION, betweenRange, "");
	}

	private SourceExprFactory withOperand(SourceExpr newOperand, int operandIndentColumn) {
		return update(opStack, newOperand, operandIndentColumn);
	}

	@Override
	public SourceExprFactory stringLiteral(FileRange range, int indentColumn, String token) {
		return visitAtom(range, indentColumn, new StringLiteral(sfr(range), indentColumn, token));
	}
	@Override
	public SourceExprFactory numberLiteral(FileRange range, int indentColumn, Number number) {
		return visitAtom(range, indentColumn, new NumberLiteral(sfr(range), indentColumn, number));
	}
	@Override
	public SourceExprFactory identifier(FileRange range, int indentColumn, String text) {
		return visitAtom(range, indentColumn, new Identifier(sfr(range), indentColumn, text));
	}

	@Override
	public SourceExprFactory operator(FileRange range, int indentColumn, String op) {
		return checkNewline(range, indentColumn).operator_(range, op, indentColumn);
	}

	private SourceExprFactory operator_(FileRange range, String op, int indentColumn) {
	    SourceExprFactory cp = tryCloseParen(range, op);
		if(cp != null) {
			return cp;
		}


		if(operand != null) {
			// We have an operand, so this is either a suffix or infix operator applying to that operand.
			SourceExprFactory suf = suffixOperator(range, op);
			if(suf != null) return suf;

			SourceExprFactory infix = infixOperator(range, op, indentColumn);
			if(infix != null) return infix;

		} else {
			// Operator after . is automatically promoted to an identifier for the binary version
			if(opStack.isNotEmpty()) {
				final Operator topOp = opStack.head().getOperator();
				switch(topOp) {
				case PROJECTION:
				case SELECTOR:
				case BASE_SLOT:
				case PROJECTION_OF_MEMBERS:

					// Try for an operator method - where operators change meaning based on position,
					// favor binary over unary, and prefix over suffix.  People who want to reference
					// those other methods will have to use backslashes.
					for(Position p : new Position[] {Position.INFIX, Position.PREFIX, Position.SUFFIX}) {
						Operator oper = Operator.fromOp(op, p);
						if(oper != null && oper.getMethodName() != null && !oper.isParen())
							return identifier(range, indentColumn, oper.getMethodName());
					}
					break;
				default:
					// Not application for other operators
					break;
				}
			}
			SourceExprFactory prefix = prefixOperator(range, indentColumn, op);
			if(prefix != null) return prefix;
		}

		return visitAtom(range, indentColumn, new BadIdentifier(sfr(range), "Unsupported operator: '"+op+"'", op));
    }

	public SourceExprFactory pushSuffixOperator(FileRange range, String op, Operator operator) {
		return update(new UnaryOp(
				operator,
				List.single(sfr(range)),
				operand),
			operandIndentColumn);
	}
	public SourceExprFactory suffixOperator(FileRange range, String op) {
			// Infix or suffix position
			final Operator operator = Operator.fromOp(op, Position.SUFFIX);
			if(operator != null) {
				return applyOperatorPrecedenceToStack(operator)
						.pushSuffixOperator(range, op, operator);
			} else {
				return null;
			}
	}

	public SourceExprFactory pushPartialBinaryOp(Operator operator, FileRange range, String op) {
		if(operand == null) throw new NullPointerException();
		return pushPartialOp(new PartialBinaryOp(
				operator,
				operand,
				new OperatorRef(sfr(range), operandIndentColumn, op),
				operandIndentColumn));
	}
	public SourceExprFactory infixOperator(FileRange range, String op, int indentColumn) {
		Operator operator = Operator.fromOp(op, Position.INFIX);

		if(operator != null) {
			// Current operand is the rightmost operand for anything of higher precedence than the operator we just got.
			return applyOperatorPrecedenceToStack(operator)
					.pushPartialBinaryOp(operator, range, op);
		} else {
			// Maybe a juxtaposition with a unary operator (like parens)?
			return juxtaposition(range).prefixOperator(range, indentColumn, op);
		}
	}

	public SourceExprFactory prefixOperator(FileRange range, int indentColumn, String op) {
		// Prefix position
		Operator operator = Operator.fromOp(op, Position.PREFIX);
		if(operator != null) {
			return pushPartialOp(new PartialUnaryOp(sfr(range), operator, indentColumn));
		}
		return null;
	}

	private int operandIndentColumn(Operator operator, FileRange range) {
		int indentColumn = range.getStartColumn();

		// Paren allows its content to dedent past the left
		// operand, but not past the start of the line the
		// left operand is on.
		final SourceExpr operand = this.operand;
		final int startLine = range.getStartLine();
		if(operand != null) {
			for(SourceFileRange r : operand.getSourceFileRanges()) {
				if(r.getStartLine() == startLine) {
					indentColumn = this.operandIndentColumn;
				}
			}
		}
		if(!opStack.isEmpty()) {
			for(final PartialOp pop : opStack) {
				for(SourceFileRange r : pop.ranges) {
					if(r.getStartLine() == startLine) {
						indentColumn = pop.indentColumn;
					}
				}
			}
		}
		return indentColumn;
	}

	private SourceExprFactory update(SourceExpr operand, int operandIndentColumn) {
		return update(opStack, operand, operandIndentColumn);
	}

	public SourceExprFactory applyOperatorPrecedenceToStack(Operator operator) {
		SourceExpr operand = this.operand;
		final Precedence prec = operator.getLeftPrecedence();
		final boolean rightAssoc = operator.isRightAssociative();
		List<PartialOp> opStack = this.opStack;
		int operandIndentColumn = this.operandIndentColumn;
		while(!opStack.isEmpty()
				&& opStack.head().getOperator().isParen() == false
				&& !(rightAssoc && opStack.head().getOperator() == operator)
				&& (opStack.head().getPrecedence().isHigherOrEqual(prec))) {
			final PartialOp op = opStack.head();
			opStack = opStack.tail();
			operand = op.rhs(operand, operandIndentColumn);
			operandIndentColumn = op.indentColumn;
		}
		return update(opStack, operand, operandIndentColumn);
	}

	private SourceExprFactory tryCloseParen(FileRange range, String op) {
		if(op.length() != 1)
			return null;


		final ParenType closeParenTypeMaybe = ParenType.forCloseChar(op.charAt(0));

		if(closeParenTypeMaybe == null)
			return null;

		// If there is a trailing comma, semicolon, or newline we can pop it off the stack
		final ParenType closeParenType = closeParenTypeMaybe;

		// When the open and close paren are the same, we can't match, so only check for
		// a close paren if we're previously encountered an open paren.
		List<PartialOp> opStack = this.opStack;
		if(closeParenType.getStartChar() == closeParenType.getEndChar()) {
			boolean found = false;
			for(final PartialOp po : opStack) {
				if(po.isOpenParen(closeParenType)) {
					found = true;
					break;
				}
			}
			if(!found)
				return null;
		}

		SourceExpr operand = this.getOperand();
		int operandIndentColumn = this.operandIndentColumn;
		while(!opStack.isEmpty()) {
			final PartialOp po = opStack.head();
			opStack = opStack.tail();
			if(operand == null) {
				if(po.getOperator() == Operator.NEWLINE || po.getOperator() == Operator.JUXTAPOSITION) {
					operand = ((PartialBinaryOp)po).leftOperand;
					continue;
				} else {
					operand = new EmptyExpr(sfr(FileRange.between(po.ranges.last().getFileRange(), range)));
				}
				operandIndentColumn = po.indentColumn;
			}
			if(po.isOpenParen(closeParenType)) {
				return update(opStack, po.closeParen(operand, range, operandIndentColumn), po.indentColumn);
			}
			if(po.isParen()) {
				operand = new BadSourceExpr.MismatchedCloseParen(List.single(sfr(range)), po.getParenType(), closeParenType);
				operandIndentColumn = po.indentColumn;
				break;
			} else {
				operand = po.rhs(operand, operandIndentColumn);
				operandIndentColumn = po.indentColumn;
			}
		}
		operand = new BadSourceExpr.MismatchedCloseParen(List.single(sfr(range)), closeParenType);
		return update(opStack, operand, operandIndentColumn);
	}

	@Override
	public SourceExprFactory whitespace(FileRange range, String text) {
		return this;
	}
	@Override
	public SourceExprFactory comment(FileRange range, String text) {
		return this;
	}

	@Override
	public SourceExprFactory eof(FileRange entireFileRange) {
		SourceExpr operand = this.getOperand();
		// No operand?  Treat it as an empty expression for now
		if(operand == null) {
			operand = new EmptyExpr(sfr(entireFileRange));
		}
		int operandIndentColumn = this.operandIndentColumn;
		List<PartialOp> opStack = this.opStack;
		while(!opStack.isEmpty()) {
			final PartialOp po = opStack.head();
			opStack = opStack.tail();
			operand = po.rhs(operand, operandIndentColumn);
			operandIndentColumn = po.indentColumn;
			if(po.isParen()) {
				// Insert missing close paren error, if we are missing the close paren
				operand = new BinaryOp(Operator.EXTEND, new MissingCloseParen(po.operatorExpr.getSourceFileRanges(), po.getParenType()), operand);
			}
		}
		return update(opStack, operand, operand, 0);
	}

	@Override
	public SourceExprFactory badToken(FileRange fileRange, String text, String message) {
		return visitAtom(fileRange, fileRange.getStartColumn(), new BadSourceExpr(sfr(fileRange), message));
	}

	private final SourceFileRange sfr(FileRange fileRange) {
		return new SourceFileRange(this.sourceFile, fileRange);
	}

	public SourceExpr getOperand() {
		return operand;
	}

	public SourceExpr getResult() {
		return result;
	}

}
