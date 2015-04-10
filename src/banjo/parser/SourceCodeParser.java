package banjo.parser;

import java.io.IOException;

import banjo.dom.ParenType;
import banjo.dom.source.BadSourceExpr;
import banjo.dom.source.BadSourceExpr.MissingCloseParen;
import banjo.dom.source.BaseSourceExprVisitor;
import banjo.dom.source.BinaryOp;
import banjo.dom.source.EmptyExpr;
import banjo.dom.source.Operator;
import banjo.dom.source.Operator.Position;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExpr;
import banjo.dom.source.UnaryOp;
import banjo.dom.token.BadIdentifier;
import banjo.dom.token.Identifier;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;
import banjo.dom.token.TokenVisitor;
import banjo.parser.util.FileRange;
import banjo.parser.util.ParserReader;
import banjo.parser.util.SourceFileRange;
import fj.data.List;

/**
 * Change input into an AST.
 */
public class SourceCodeParser implements TokenVisitor<SourceCodeParser> {
	private static final String DEFAULT_ID_PREFIX = "/temp/";
	public final String sourceFile;
	public final List<PartialOp> opStack;
	public final SourceExpr operand;
	public final SourceExpr result;
	public final int operandIndentLevel;

	abstract class PartialOp {
		protected final Operator operator;
		protected final List<SourceFileRange> ranges;
		protected final SourceExpr operatorExpr;
		protected final int indentLevel;

		/**
		 *
		 */
		public PartialOp(List<SourceFileRange> ranges, Operator operator, SourceExpr operatorExpr, int indentLevel) {
			this.ranges = ranges.append(operatorExpr.getSourceFileRanges()).sort(SourceFileRange.ORD);
			this.operator = operator;
			this.operatorExpr = operatorExpr;
			this.indentLevel = indentLevel;
		}

		/**
		 * Create a node for a non-parentheses operator with a right-hand expression.
		 */
		public SourceExpr rhs(SourceExpr rightOperand) {
			final List<SourceFileRange> ranges = SourceFileRange.append(this.ranges, rightOperand.getSourceFileRanges());
			return _rhs(rightOperand, ranges, this.operatorExpr.getSourceFileRanges());
		}

		/**
		 * Create a node for a parenthesized expression.
		 */
		public SourceExpr closeParen(SourceExpr rightOperand, FileRange closeParenRange) {
			final List<SourceFileRange> operatorRanges = SourceFileRange.snoc(this.operatorExpr.getSourceFileRanges(), sfr(closeParenRange));
			final List<SourceFileRange> ranges = this.ranges.snoc(sfr(closeParenRange));
			return _rhs(rightOperand, ranges, operatorRanges);
		}

		private SourceExpr _rhs(SourceExpr rightOperand, final List<SourceFileRange> ranges, final List<SourceFileRange> operatorRanges) {
			if(this.operator == Operator.INVALID) {
				return new BadSourceExpr.UnsupportedOperator(ranges, this.operatorExpr.toSource());
			}
			if(badIndentation(rightOperand)) {
				rightOperand = new BadSourceExpr.IncorrectIndentation(
						rightOperand.getSourceFileRanges(),
						rightOperand.getSourceFileRanges().head().getStartColumn(),
						ranges.head().getStartColumn()
				);
			}
			return makeOp(rightOperand, ranges, operatorRanges);
		}

		public boolean badIndentation(SourceExpr rightOperand) {
			return false;
//			return rightOperand.getSourceFileRanges().isNotEmpty() &&
//					this.ranges.isNotEmpty() &&
//					rightOperand.getSourceFileRanges().head().getStartColumn() < ranges.head().getStartColumn() && !(this.operator.isParen() || this.operator.isSuffix());
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
		public PartialBinaryOp(Operator operator, SourceExpr operand, SourceExpr operatorExpr, int indentLevel) {
			super(operand.getSourceFileRanges().append(operatorExpr.getSourceFileRanges()),
					operator, operatorExpr, indentLevel);
			this.leftOperand = operand;
		}

		public PartialBinaryOp(Operator operator, SourceExpr operand, SourceFileRange operatorRange, int indentLevel) {
			this(operator, operand, new OperatorRef(operatorRange, operator.getOp()), indentLevel);
		}

		@Override
		public SourceExpr makeOp(SourceExpr rightOperand, List<SourceFileRange> ranges, List<SourceFileRange> operatorRanges) {
			return new BinaryOp(ranges, this.operator, operatorRanges, this.leftOperand, rightOperand);
		}

		@Override
		public boolean badIndentation(SourceExpr rightOperand) {
			return super.badIndentation(rightOperand) && Boolean.FALSE == this.leftOperand.acceptVisitor(new BaseSourceExprVisitor<Boolean>() {
				@Override
				public Boolean binaryOp(BinaryOp op) {
					return op.getOperator().isParen();
				}

				@Override
				public Boolean unaryOp(UnaryOp op) {
					return op.getOperator().isParen();
				}

				@Override
				public Boolean fallback(SourceExpr other) {
					return false;
				}
			});
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

		public PartialUnaryOp(List<SourceFileRange> ranges, Operator operator, int indentLevel) {
			super(ranges, operator, new OperatorRef(ranges, operator.getOp()), indentLevel);
		}

		public PartialUnaryOp(SourceFileRange range, Operator operator, int indentLevel) {
			this(List.single(range), operator, indentLevel);
		}

		@Override
		SourceExpr makeOp(SourceExpr rightOperand, List<SourceFileRange> ranges, List<SourceFileRange> operatorRanges) {
			return new UnaryOp(ranges, this.operator, operatorRanges, rightOperand);
		}

		@Override
		public String toString() {
			if(this.operator.isParen())
				return this.operator.getParenType().getStartChar() +"_"+this.operator.getParenType().getEndChar();
			return "("+this.operator.getOp()+" _)";
		}
	}

	public SourceCodeParser(String sourceFile, List<PartialOp> opStack, SourceExpr operand, SourceExpr result, int operandIndentLevel) {
		super();
		this.sourceFile = sourceFile;
		this.opStack = opStack;
		this.operand = operand;
		this.result = result;
		this.operandIndentLevel = operandIndentLevel;
	}

	/**
	 * Construct a parser with the given string as the source file name to use when
	 * reporting errors.
	 */
	public SourceCodeParser(String sourceFile) {
		this(sourceFile, List.nil(), null, null, 0);
	}

	/**
	 * Constructor that doesn't take a filename, for when you don't care about filenames
	 */
	public SourceCodeParser() {
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
			final SourceCodeScanner scanner = new SourceCodeScanner();
			SourceCodeParser parseResult = scanner.scan(in, this);
			SourceExpr operand = parseResult.getOperand();
			if(operand == null)
				return new EmptyExpr();
			return operand;
		} finally {
			in.close();
		}
	}

	public SourceExpr parse(String source) throws IOException {
		final SourceCodeScanner scanner = new SourceCodeScanner();
		SourceCodeParser parseResult = scanner.scan(source, this);
		SourceExpr operand = parseResult.getOperand();
		if(operand == null)
			return new EmptyExpr();
		return operand;
	}

	/**
	 * Helper for checkIndentDedent to decide whether an expression is closed
	 * off by a de-dent.
	 */
	boolean shouldPopBasedOnDedent(int column) {
		if(this.opStack.isEmpty()) return false;
		final PartialOp first = this.opStack.head();
		final int indentLevel = first.indentLevel;
		return !first.isParen() && first.getOperator() != Operator.NEWLINE;
		//return column < indentLevel || (!first.isParen() && column == indentLevel);
	}

	SourceCodeParser popPartialOp() {
		final SourceExpr operand = this.operand;
		if(operand == null) throw new NullPointerException();
		final PartialOp op = opStack.head();
		return update(opStack.tail(), op.rhs(operand), op.indentLevel);
	}

	SourceCodeParser pushNewline(FileRange range) {
		return applyOperatorPrecedenceToStack(Operator.NEWLINE)
				.pushPartialBinaryOp(Operator.NEWLINE, FileRange.between(operand.getSourceFileRanges().last().getFileRange(), range), "\n");
	}

	/**
	 * Check for indent/dedent prior to the given token.  May consume the contents of nodes with the assumption it is
	 * whitespace/comments only.
	 *
	 * @param range The range of the token
	 */
	private SourceCodeParser checkNewline(FileRange range) {
		final int line = range.getStartLine();
		final int column = range.getStartColumn();
		//final int offset = range.getStartOffset();
		SourceExpr operand = this.getOperand();

		// Check if we have an operand, it has source location, and we have moved to the next line
		if(operand == null || operand.getSourceFileRanges().isEmpty() || line <= operand.getSourceFileRanges().last().getEndLine())
			return this;

		// Now if we get a de-dent we have to move up the operator stack
		SourceCodeParser ps = this;
		while(ps.shouldPopBasedOnDedent(column)) {
			ps = ps.popPartialOp();
		}

		// Insert a newline operation since we moved to a new line
		return ps.pushNewline(range);
	}

	private SourceCodeParser update(List<PartialOp> opStack, SourceExpr operand, SourceExpr result, int operandIndentLevel) {
		return new SourceCodeParser(sourceFile, opStack, operand, result, operandIndentLevel);
	}
	private SourceCodeParser update(List<PartialOp> opStack, SourceExpr operand, int operandIndentLevel) {
		return update(opStack, operand, result, operandIndentLevel);
	}

	public SourceCodeParser pushPartialOp(PartialOp partialOp) {
		return update(opStack.cons(partialOp), null, 0);
	}

	private SourceCodeParser visitAtom(FileRange range, SourceExpr token) {
		return checkNewline(range)
				.maybeJuxtaposition(range)
				.withOperand(token, range.getStartColumn());
	}

	private SourceCodeParser maybeJuxtaposition(FileRange nextTokenRange) {
		if(operand != null) {
			return juxtaposition(nextTokenRange);
		}
		return this;
	}

	private SourceCodeParser juxtaposition(FileRange nextTokenRange) {
		final FileRange betweenRange = FileRange.between(operand.getSourceFileRanges().last().getFileRange(), nextTokenRange);
		return applyOperatorPrecedenceToStack(Operator.JUXTAPOSITION)
				.pushPartialBinaryOp(Operator.JUXTAPOSITION, betweenRange, "");
	}

	private SourceCodeParser withOperand(SourceExpr newOperand, int operandIndentLevel) {
		return update(opStack, newOperand, operandIndentLevel);
	}

	@Override
	public SourceCodeParser stringLiteral(FileRange range, String token) {
		return visitAtom(range, new StringLiteral(sfr(range), token));
	}
	@Override
	public SourceCodeParser numberLiteral(FileRange range, Number number, String suffix) {
		return visitAtom(range, new NumberLiteral(sfr(range), number, suffix));
	}
	@Override
	public SourceCodeParser identifier(FileRange range, String text) {
		return visitAtom(range, new Identifier(sfr(range), text));
	}

	@Override
	public SourceCodeParser operator(FileRange range, String op) {
		return checkNewline(range).operator_(range, op);
	}

	private SourceCodeParser operator_(FileRange range, String op) {
	    SourceCodeParser cp = tryCloseParen(range, op);
		if(cp != null) {
			return cp;
		}

		// Operator after . is automatically promoted to an identifier for the binary version
		if(opStack.isNotEmpty()) {
			final Operator topOp = opStack.head().getOperator();
			switch(topOp) {
			case PROJECTION:
			case SELECTOR:
			case BASE_SLOT:
				// Try for an operator method - where operators change meaning based on position,
				// favor binary over unary, and prefix over suffix.  People who want to reference
				// those other methods will have to use backslashes.
				for(Position p : new Position[] {Position.INFIX, Position.PREFIX, Position.SUFFIX}) {
					Operator oper = Operator.fromOp(op, p);
					if(oper != null && oper.getMethodName() != null)
						return identifier(range, oper.getMethodName());
				}
				break;
			default:
				// Not application for other operators
				break;
			}
		}

		if(operand != null) {
			// We have an operand, so this is either a suffix or infix operator applying to that operand.
			SourceCodeParser suf = suffixOperator(range, op);
			if(suf != null) return suf;

			SourceCodeParser infix = infixOperator(range, op);
			if(infix != null) return infix;

		} else {
			SourceCodeParser prefix = prefixOperator(range, op);
			if(prefix != null) return prefix;
		}

		return visitAtom(range, new BadIdentifier(sfr(range), "Unsupported operator: '"+op+"'", op));
    }

	public SourceCodeParser pushSuffixOperator(FileRange range, String op, Operator operator) {
		return update(new UnaryOp(
				operator,
				List.single(sfr(range)),
				operand),
			operandIndentLevel);
	}
	public SourceCodeParser suffixOperator(FileRange range, String op) {
			// Infix or suffix position
			final Operator operator = Operator.fromOp(op, Position.SUFFIX);
			if(operator != null) {
				return applyOperatorPrecedenceToStack(operator)
						.pushSuffixOperator(range, op, operator);
			} else {
				return null;
			}
	}

	public SourceCodeParser pushPartialBinaryOp(Operator operator, FileRange range, String op) {
		if(operand == null) throw new NullPointerException();
		return pushPartialOp(new PartialBinaryOp(
				operator,
				operand,
				new OperatorRef(sfr(range), op),
				operandIndentLevel(operator, range)));
	}
	public SourceCodeParser infixOperator(FileRange range, String op) {
		Operator operator = Operator.fromOp(op, Position.INFIX);

		if(operator != null) {
			// Current operand is the rightmost operand for anything of higher precedence than the operator we just got.
			return applyOperatorPrecedenceToStack(operator)
					.pushPartialBinaryOp(operator, range, op);
		} else {
			// Maybe a juxtaposition with a unary operator (like parens)?
			return juxtaposition(range).prefixOperator(range, op);
		}
	}

	public SourceCodeParser prefixOperator(FileRange range, String op) {
		// Prefix position
		Operator operator = Operator.fromOp(op, Position.PREFIX);
		if(operator != null) {
			return pushPartialOp(new PartialUnaryOp(sfr(range), operator, operandIndentLevel(operator, range)));
		}
		return null;
	}

	private int operandIndentLevel(Operator operator, FileRange range) {
		int indentLevel = range.getStartColumn();

		// Paren allows its content to dedent past the left
		// operand, but not past the start of the line the
		// left operand is on.
		final SourceExpr operand = this.operand;
		final int startLine = range.getStartLine();
		if(operand != null) {
			for(SourceFileRange r : operand.getSourceFileRanges()) {
				if(r.getStartLine() == startLine) {
					indentLevel = this.operandIndentLevel;
				}
			}
		}
		if(!opStack.isEmpty()) {
			for(final PartialOp pop : opStack) {
				for(SourceFileRange r : pop.ranges) {
					if(r.getStartLine() == startLine) {
						indentLevel = pop.indentLevel;
					}
				}
			}
		}
		return indentLevel;
	}

	private SourceCodeParser update(SourceExpr operand, int operandIndentLevel) {
		return update(opStack, operand, operandIndentLevel);
	}

	public SourceCodeParser applyOperatorPrecedenceToStack(Operator operator) {
		SourceExpr operand = this.operand;
		final Precedence prec = operator.getLeftPrecedence();
		final boolean rightAssoc = operator.isRightAssociative();
		List<PartialOp> opStack = this.opStack;
		int operandIndentLevel = this.operandIndentLevel;
		while(!opStack.isEmpty()
				&& opStack.head().getOperator().isParen() == false
				&& !(rightAssoc && opStack.head().getOperator() == operator)
				&& (opStack.head().getPrecedence().isHigherOrEqual(prec))) {
			final PartialOp op = opStack.head();
			opStack = opStack.tail();
			operand = op.rhs(operand);
			operandIndentLevel = op.indentLevel;
		}
		return update(opStack, operand, operandIndentLevel);
	}

	private SourceCodeParser tryCloseParen(FileRange range, String op) {
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
		int operandIndentLevel = this.operandIndentLevel;
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
				operandIndentLevel = po.indentLevel;
			}
			if(po.isOpenParen(closeParenType)) {
				return update(opStack, po.closeParen(operand, range), po.indentLevel);
			}
			if(po.isParen()) {
				operand = new BadSourceExpr.MismatchedCloseParen(List.single(sfr(range)), po.getParenType(), closeParenType);
				operandIndentLevel = po.indentLevel;
				break;
			} else {
				operand = po.rhs(operand);
				operandIndentLevel = po.indentLevel;
			}
		}
		operand = new BadSourceExpr.MismatchedCloseParen(List.single(sfr(range)), closeParenType);
		return update(opStack, operand, operandIndentLevel);
	}

	@Override
	public SourceCodeParser whitespace(FileRange range, String text) {
		return this;
	}
	@Override
	public SourceCodeParser comment(FileRange range, String text) {
		return this;
	}

	@Override
	public SourceCodeParser eof(FileRange entireFileRange) {
		SourceExpr operand = this.getOperand();
		// No operand?  Treat it as an empty expression for now
		if(operand == null) {
			operand = new EmptyExpr(sfr(entireFileRange));
		}
		List<PartialOp> opStack = this.opStack;
		while(!opStack.isEmpty()) {
			final PartialOp po = opStack.head();
			opStack = opStack.tail();
			if(po.isParen()) {
				operand = new MissingCloseParen(po.operatorExpr.getSourceFileRanges(), po.getParenType());
			} else {
				operand = po.rhs(operand);
			}
		}
		return update(opStack, operand, operand, 0);
	}

	@Override
	public SourceCodeParser badToken(FileRange fileRange, String text, String message) {
		return visitAtom(fileRange, new BadSourceExpr(sfr(fileRange), message));
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
