package banjo.parser;

import static banjo.parser.util.Check.nonNull;

import java.io.IOException;
import java.util.LinkedList;

import org.eclipse.jdt.annotation.Nullable;

import fj.data.List;
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
import banjo.dom.token.Atom;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;
import banjo.dom.token.TokenVisitor;
import banjo.parser.util.FileRange;
import banjo.parser.util.ParserReader;
import banjo.parser.util.SourceFileRange;
import banjo.util.SourceNumber;

/**
 * Change input into an AST.
 */
public class SourceCodeParser implements TokenVisitor<SourceCodeParser> {
	private final String sourceFile;
	private final List<PartialOp> opStack;
	private final @Nullable SourceExpr operand;
	private final @Nullable SourceExpr result;

	abstract class PartialOp {
		protected final Operator operator;
		protected final List<SourceFileRange> ranges;
		protected final SourceExpr operatorExpr;

		/**
		 *
		 * @param range
		 * @param nodes SourceNode instances that make up the source expression.  They must be continuous (no gaps)
		 * @param operator
		 * @param problems
		 */
		public PartialOp(List<SourceFileRange> ranges, Operator operator, SourceExpr operatorExpr) {
			this.ranges = ranges;
			this.operator = operator;
			this.operatorExpr = operatorExpr;
		}

		/**
		 * Create a node for a non-parentheses operator with a right-hand expression.
		 */
		public SourceExpr rhs(SourceExpr rightOperand) {
			final List<SourceFileRange> ranges = this.ranges.append(rightOperand.getSourceFileRanges());
			return _rhs(rightOperand, ranges);
		}

		/**
		 * Create a node for a parenthesized expression.
		 */
		public SourceExpr closeParen(SourceExpr rightOperand, FileRange closeParenRange) {
			final List<SourceFileRange> ranges = this.ranges.snoc(sfr(closeParenRange));
			return _rhs(rightOperand, ranges);
		}

		private SourceExpr _rhs(SourceExpr rightOperand, final List<SourceFileRange> ranges) {
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
			return makeOp(rightOperand);
		}

		public boolean badIndentation(SourceExpr rightOperand) {
			return rightOperand.getSourceFileRanges().isNotEmpty() &&
					this.ranges.isNotEmpty() &&
					rightOperand.getSourceFileRanges().head().getStartColumn() < ranges.head().getStartColumn() && !(this.operator.isParen() || this.operator.isSuffix());
		}

		/**
		 * Create a node for a non-parentheses operator with a right-hand expression.
		 */
		abstract SourceExpr makeOp(SourceExpr sourceExpr);

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
		public PartialBinaryOp(Operator operator, SourceExpr operand, SourceExpr operatorExpr) {
			super(operand.getSourceFileRanges().append(operatorExpr.getSourceFileRanges()),
					operator, operatorExpr);
			this.leftOperand = operand;
		}

		public PartialBinaryOp(Operator operator, SourceExpr operand, SourceFileRange operatorRange) {
			this(operator, operand, new OperatorRef(operatorRange, operator.getOp()));
		}

		@Override
		public SourceExpr makeOp(SourceExpr rightOperand) {
			return new BinaryOp(this.ranges, this.operator, nonNull(this.operatorExpr.getSourceFileRanges().head()), this.leftOperand, rightOperand);
		}

		@Override
		public boolean badIndentation(SourceExpr rightOperand) {
			return super.badIndentation(rightOperand) && Boolean.FALSE == this.leftOperand.acceptVisitor(new BaseSourceExprVisitor<Boolean>() {
				@SuppressWarnings("null")
				@Override
				public Boolean binaryOp(BinaryOp op) {
					return op.getOperator().isParen();
				}

				@SuppressWarnings("null")
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

		public PartialUnaryOp(List<SourceFileRange> ranges, Operator operator) {
			super(ranges, operator, new OperatorRef(ranges, operator.getOp()));
		}

		public PartialUnaryOp(SourceFileRange range, Operator operator) {
			this(List.single(range), operator);
		}

		@Override
		SourceExpr makeOp(SourceExpr rightOperand) {
			return new UnaryOp(this.ranges, this.operator, nonNull(this.operatorExpr.getSourceFileRanges().head()), rightOperand);
		}

		@Override
		public String toString() {
			return "("+this.operator.getOp()+" _)";
		}
	}

	public SourceCodeParser(String sourceFile, List<PartialOp> opStack, @Nullable SourceExpr operand, @Nullable SourceExpr result) {
		super();
		this.sourceFile = sourceFile;
		this.opStack = opStack;
		this.operand = operand;
		this.result = result;
	}

	/**
	 * Construct a parser with the given string as the source file name to use when
	 * reporting errors.
	 */
	public SourceCodeParser(String sourceFile) {
		this(sourceFile, List.<PartialOp>nil(), null, null);
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
			reset();
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
		reset();
		final SourceCodeScanner scanner = new SourceCodeScanner();
		SourceCodeParser parseResult = nonNull(scanner.scan(source, this));
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
		if(first.ranges.isEmpty())
			return false; // Can't pop based on dedent for a synthetic node
		int startColumn = first.ranges.head().getStartColumn();
		if(first.getOperator() == Operator.NEWLINE) {
			return column < startColumn;
		}
		if(first.isParen()) {
			// Paren allows de-dent, but not past the parent expressions on the same line as the open paren
			final int startLine = first.ranges.head().getStartLine();
			for(final PartialOp pop : this.opStack) {
				if(pop.ranges.isEmpty())
					break;
				if(pop.ranges.last().getEndLine() != startLine) {
					break;
				}
				startColumn = pop.ranges.head().getStartColumn();
			}
			return column < startColumn;
		}
		return column <= startColumn;
	}
	/**
	 * Check for indent/dedent prior to the given token.  May consume the contents of nodes with the assumption it is
	 * whitespace/comments only.
	 *
	 * @param range The range of the token
	 * @param token The token we just encountered (should not be whitespace or a comment)
	 */
	private SourceCodeParser checkIndentDedent(FileRange range) {
		final int line = range.getStartLine();
		final int column = range.getStartColumn();
		//final int offset = range.getStartOffset();
		SourceExpr operand = this.getOperand();

		// Check if we have an operand, it has source location, and we have moved to the next line
		if(operand == null || operand.getSourceFileRanges().isEmpty() || line <= operand.getSourceFileRanges().head().getEndLine())
			return this;

		// Now if we get a de-dent we have to move up the operator stack
		SourceCodeParser ps = this;
		while(ps.shouldPopBasedOnDedent(column)) {
			final PartialOp op = ps.opStack.head();
			ps = ps.update(opStack.tail(), op.rhs(operand));
		}
		operand = ps.operand;
		if(operand == null)
			return this;

		// If we de-dented back to an exact match on the column of an enclosing
		// expression, insert a newline operator
		if(operand.getSourceFileRanges().isNotEmpty() && operand.getSourceFileRanges().head().getStartColumn() == column) {
			ps = ps.pushPartialOp(new PartialBinaryOp(Operator.NEWLINE, operand, sfr(FileRange.between(operand.getSourceFileRanges().last().getFileRange(), range))));
		}
		return ps;
	}

	private SourceCodeParser update(List<PartialOp> opStack, @Nullable SourceExpr operand, @Nullable SourceExpr result) {
		return new SourceCodeParser(sourceFile, opStack, operand, result);
	}
	private SourceCodeParser update(List<PartialOp> opStack, @Nullable SourceExpr operand) {
		return update(opStack, operand, result);
	}

	public SourceCodeParser pushPartialOp(PartialOp partialOp) {
		return update(opStack.cons(partialOp), null);
	}

	private SourceCodeParser visitAtom(FileRange range, SourceExpr token) {
		SourceCodeParser ps = checkIndentDedent(range);
		final SourceExpr operand = ps.operand;
		if(operand != null) {
			final SourceFileRange betweenRange = operand.getSourceFileRanges().isEmpty() ? sfr(range.headRange()) : sfr(FileRange.between(operand.getSourceFileRanges().last().getFileRange(), range));
			ps = ps.pushPartialOp(new PartialBinaryOp(Operator.JUXTAPOSITION, operand, betweenRange));
		}
		return ps.withOperand(token);
	}
	private SourceCodeParser withOperand(SourceExpr newOperand) {
		return update(opStack, newOperand);
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
		SourceCodeParser ps = checkIndentDedent(range);

		SourceCodeParser cp = tryCloseParen(range, op);
		if(cp != null) {
			return cp;
		}
		SourceExpr operand = this.getOperand();
		if(operand != null) {
			// Infix or suffix position
			@Nullable
			final Operator suffixOperator = Operator.fromOp(op, Position.SUFFIX);
			if(suffixOperator != null) {
				ps = applyOperatorPrecedenceToStack(operand, suffixOperator); // Apply operator precedence
				operand = nonNull(ps.getOperand());
				final List<SourceFileRange> exprRanges = operand.getSourceFileRanges().snoc(sfr(range));
				return ps.update(new UnaryOp(exprRanges, suffixOperator, sfr(range), operand));
			}
			@Nullable Operator operator = Operator.fromOp(op, Position.INFIX);

			if(operator == null) {
				operator = Operator.INVALID;
			}
			// Current operand is the rightmost operand for anything of higher precedence than the operator we just got.
			ps = applyOperatorPrecedenceToStack(operand, operator);
			operand = nonNull(ps.getOperand());
			return ps.pushPartialOp(new PartialBinaryOp(operator, operand, new OperatorRef(sfr(range), op)));
		} else {
			// Prefix position
			@Nullable Operator operator = Operator.fromOp(op, Position.PREFIX);
			if(operator == null) {
				operator = Operator.INVALID;
			}
			return ps.pushPartialOp(new PartialUnaryOp(sfr(range), nonNull(operator)));
		}
	}

	private SourceCodeParser update(SourceExpr operand) {
		return update(opStack, operand);
	}

	public SourceCodeParser applyOperatorPrecedenceToStack(SourceExpr operand,
			Operator operator) {
		final Precedence prec = operator.getLeftPrecedence();
		final boolean rightAssoc = operator.isRightAssociative();
		List<PartialOp> opStack = this.opStack;
		while(!opStack.isEmpty()
				&& opStack.head().getOperator().isParen() == false
				&& !(rightAssoc && opStack.head().getOperator() == operator)
				&& (opStack.head().getPrecedence().isHigherOrEqual(prec))) {
			final PartialOp op = opStack.head();
			opStack = opStack.tail();
			operand = op.rhs(operand);
		}
		return update(opStack, operand);
	}

	private @Nullable SourceCodeParser tryCloseParen(FileRange range, String op) {
		if(op.length() != 1)
			return null;

		@Nullable
		final ParenType closeParenTypeMaybe = ParenType.forCloseChar(op.charAt(0));

		if(closeParenTypeMaybe == null)
			return null;

		// If there is a trailing comma, semicolon, or newline we can pop it off the stack
		final ParenType closeParenType = nonNull(closeParenTypeMaybe);

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
		while(!opStack.isEmpty()) {
			final PartialOp po = opStack.head();
			opStack = opStack.tail();
			if(operand == null) {
				operand = new EmptyExpr(sfr(FileRange.between(po.ranges.last().getFileRange(), range)));
			}
			if(po.isOpenParen(closeParenType)) {
				return update(opStack, po.closeParen(operand, range));
			}
			if(po.isParen()) {
				final List<SourceFileRange> newRanges = operand.getSourceFileRanges().snoc(sfr(range));
				operand = new BadSourceExpr.MismatchedCloseParen(newRanges, closeParenType);
				break;
			} else {
				operand = po.rhs(operand);
			}
		}
		return update(opStack, operand);
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
				operand = po.rhs(nonNull(operand));
			}
		}
		return update(opStack, operand, operand);
	}

	public SourceCodeParser reset() {
		return update(List.<PartialOp>nil(), null, null);
	}

	@Override
	public SourceCodeParser badToken(FileRange fileRange, String text, String message) {
		return visitAtom(fileRange, new BadSourceExpr(sfr(fileRange), message));
	}

	private final SourceFileRange sfr(FileRange fileRange) {
		return new SourceFileRange(this.sourceFile, fileRange);
	}

	public @Nullable SourceExpr getOperand() {
		return operand;
	}

	public @Nullable SourceExpr getResult() {
		return result;
	}

}
