package banjo.parser;

import static banjo.parser.util.Check.nonNull;

import java.io.IOException;
import java.util.LinkedList;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.ParenType;
import banjo.dom.source.BadSourceExpr;
import banjo.dom.source.BaseSourceExprVisitor;
import banjo.dom.source.BinaryOp;
import banjo.dom.source.EmptyExpr;
import banjo.dom.source.Operator;
import banjo.dom.source.Operator.Position;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExpr;
import banjo.dom.source.UnaryOp;
import banjo.dom.token.BadToken;
import banjo.dom.token.Comment;
import banjo.dom.token.Ellipsis;
import banjo.dom.token.Identifier;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;
import banjo.dom.token.TokenVisitor;
import banjo.dom.token.Whitespace;
import banjo.parser.BanjoParser.ExtSourceExpr;
import banjo.parser.util.FileRange;
import banjo.parser.util.ParserReader;
import banjo.parser.util.SourceMap;

/**
 * Change input into an AST.
 */
public class BanjoParser implements TokenVisitor<ExtSourceExpr> {

	private final LinkedList<PartialOp> opStack = new LinkedList<>();
	private @Nullable ExtSourceExpr operand = null;
	private boolean eof;
	static final SourceMap EMPTY_SOURCE_MAP = new SourceMap();

	public static class ExtSourceExpr {
		private final FileRange range;
		private final SourceMap sourceMap;
		private final SourceExpr expr;

		/**
		 * Create one from a SourceExpr with a file range and source maps for all its children
		 */
		public ExtSourceExpr(SourceExpr expr, FileRange range, SourceMap sourceMap) {
			this.range = range;
			this.expr = expr;
			this.sourceMap = sourceMap.insert(expr, range);
		}
		/**
		 * Create one from an atom.  The expression should be made of just one SourceNode and have no
		 * children.
		 */
		public ExtSourceExpr(FileRange range, SourceExpr expr) {
			this.range = range;
			this.expr = expr;
			this.sourceMap = EMPTY_SOURCE_MAP.insert(expr, range);
		}
		public FileRange getFileRange() {
			return this.range;
		}
		public SourceExpr getExpr() {
			return this.expr;
		}
		public int getStartColumn() {
			return this.range.getStartColumn();
		}
		public int getEndLine() {
			return this.range.getEndLine();
		}
		@Override
		public String toString() {
			return this.range+": "+this.expr;
		}
		public SourceMap getSourceMap() {
			return this.sourceMap;
		}
	}

	abstract class PartialOp {
		protected final Operator operator;
		protected final FileRange range;
		protected final SourceMap sourceMap;
		protected final ExtSourceExpr operatorExpr;

		/**
		 * 
		 * @param range
		 * @param nodes SourceNode instances that make up the source expression.  They must be continuous (no gaps)
		 * @param operator
		 * @param problems
		 */
		public PartialOp(FileRange range, SourceMap sourceMap, Operator operator, ExtSourceExpr operatorExpr) {
			this.sourceMap = sourceMap;
			this.range = range;
			this.operator = operator;
			this.operatorExpr = operatorExpr;
		}

		/**
		 * Create a node for a non-parentheses operator with a right-hand expression.
		 */
		public ExtSourceExpr rhs(ExtSourceExpr rightOperand) {
			final FileRange range = this.range.extend(rightOperand.getFileRange());
			return _rhs(rightOperand, range);
		}

		/**
		 * Create a node for a parenthesized expression.
		 */
		public ExtSourceExpr closeParen(ExtSourceExpr rightOperand, FileRange closeParenRange) {
			final FileRange range = this.range.extend(closeParenRange);
			return _rhs(rightOperand, range);
		}

		private ExtSourceExpr _rhs(ExtSourceExpr rightOperand, final FileRange range) {
			if(this.operator == Operator.INVALID) {
				return new ExtSourceExpr(new BadSourceExpr.UnsupportedOperator(this.operatorExpr.expr.toSource()), range, this.sourceMap.union(rightOperand.getSourceMap()));
			}
			if(badIndentation(rightOperand)) {
				rightOperand = new ExtSourceExpr(new BadSourceExpr.IncorrectIndentation(rightOperand.getStartColumn(), getStartColumn()), rightOperand.range, rightOperand.getSourceMap());
			}
			final SourceExpr newExpr = makeOp(rightOperand.getExpr());
			final SourceMap sourceMap = this.sourceMap.union(rightOperand.getSourceMap()).insert(newExpr, range);
			return new ExtSourceExpr(newExpr, range, sourceMap);
		}

		public boolean badIndentation(ExtSourceExpr rightOperand) {
			return rightOperand.getStartColumn() < getStartColumn() && !(this.operator.isParen() || this.operator.isSuffix());
		}

		/**
		 * Create a node for a non-parentheses operator with a right-hand expression.
		 */
		abstract SourceExpr makeOp(SourceExpr sourceExpr);

		public int getStartColumn() {
			return this.range.getStartColumn();
		}
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

		public int getStartLine() {
			return this.range.getStartLine();
		}

		public int getEndLine() {
			return this.range.getEndLine();
		}
	}
	class PartialBinaryOp extends PartialOp {
		private final SourceExpr leftOperand;

		/**
		 * Create a partial binary op.  We have the left-hand operand and the the operator but we're waiting for the right-hand
		 * operand.
		 */
		public PartialBinaryOp(Operator operator, ExtSourceExpr operand, ExtSourceExpr operatorExpr) {
			super(operand.getFileRange().extend(operatorExpr.getFileRange()),
					operand.getSourceMap().union(operatorExpr.getSourceMap()),
					operator, operatorExpr);
			this.leftOperand = operand.getExpr();
		}

		public PartialBinaryOp(Operator operator, ExtSourceExpr operand, FileRange operatorRange) {
			this(operator, operand, new ExtSourceExpr(operatorRange, new OperatorRef(operator.getOp())));
		}

		@Override
		public SourceExpr makeOp(SourceExpr rightOperand) {
			return new BinaryOp(this.operator, this.leftOperand, rightOperand);
		}

		@Override
		public boolean badIndentation(ExtSourceExpr rightOperand) {
			return super.badIndentation(rightOperand) && !nonNull(rightOperand.getExpr().acceptVisitor(new BaseSourceExprVisitor<Boolean>() {
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
			}));
		}
		@Override
		public String toString() {
			return "(" + getOperand().toSource()+" "+this.operator.getOp() +" _)";
		}
		public SourceExpr getOperand() {
			return this.leftOperand;
		}

		public FileRange getFileRange() {
			return this.range;
		}
	}

	class PartialUnaryOp extends PartialOp {

		public PartialUnaryOp(FileRange range, Operator operator) {
			super(range, EMPTY_SOURCE_MAP, operator, new ExtSourceExpr(new OperatorRef(operator.getOp()), range, EMPTY_SOURCE_MAP));
		}

		@Override
		SourceExpr makeOp(SourceExpr rightOperand) {
			//			if(this.operator == Operator.NEGATE && rightOperand instanceof NumberLiteral) {
			//				return ((NumberLiteral)rightOperand).negate();
			//			}
			//			if(this.operator == Operator.PLUS && rightOperand instanceof NumberLiteral) {
			//				return rightOperand;
			//			}
			return new UnaryOp(this.operator, rightOperand);
		}

		@Override
		public String toString() {
			return "("+this.operator.getOp()+" _)";
		}
	}

	/**
	 * Parse the input fully.  The final return value is the result of visitEof().  It may be null
	 * if there were syntax errors in the source file, or the file was empty;
	 * 
	 * @param in Input source to read; will be closed by this function
	 * @return Root of the parsed syntax tree
	 * @throws IOException If the underlying stream throws IOException
	 */
	public ExtSourceExpr parse(ParserReader in) throws IOException {
		try {
			reset();
			final BanjoScanner scanner = new BanjoScanner();
			final ExtSourceExpr result = nonNull(scanner.scan(in, this));
			return result;
		} finally {
			in.close();
		}
	}

	public ExtSourceExpr parse(String source) throws IOException {
		reset();
		final BanjoScanner scanner = new BanjoScanner();
		return nonNull(scanner.scan(source, this));
	}

	/**
	 * @return true iff we have reached the end of the input
	 */
	public boolean reachedEof() {
		return this.eof;
	}

	/**
	 * Helper for checkIndentDedent to decide whether an expression is closed
	 * off by a de-dent.
	 */
	boolean shouldPopBasedOnDedent(int column) {
		if(this.opStack.isEmpty()) return false;
		final PartialOp first = this.opStack.getFirst();
		int startColumn = first.getStartColumn();
		if(first.getOperator() == Operator.NEWLINE) {
			return column < startColumn;
		}
		if(first.isParen()) {
			// Paren allows de-dent, but not past the parent expressions on the same line as the open paren
			for(final PartialOp pop : this.opStack) {
				if(pop.getEndLine() != first.getStartLine()) {
					break;
				}
				startColumn = pop.getStartColumn();
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
	private void checkIndentDedent(FileRange range, SourceExpr token) {
		final int line = range.getStartLine();
		final int column = range.getStartColumn();
		//final int offset = range.getStartOffset();
		ExtSourceExpr operand = this.operand;
		if(operand != null && line > operand.getEndLine()) {
			// Now if we get a de-dent we have to move up the operator stack
			while(shouldPopBasedOnDedent(column)) {
				final PartialOp op = this.opStack.pop();
				this.operand = operand = op.rhs(operand);
			}

			// If we de-dented back to an exact match on the column of an enclosing
			// expression, insert a newline operator
			if(operand.getStartColumn() == column) {
				pushPartialOp(new PartialBinaryOp(Operator.NEWLINE, operand, FileRange.between(operand.getFileRange(), range)));
			}
		}
	}

	public void pushPartialOp(PartialOp partialOp) {
		this.opStack.push(partialOp);
		this.operand = null;
	}

	@Nullable
	private ExtSourceExpr visitAtom(FileRange range, SourceExpr token) {
		checkIndentDedent(range, token);
		final ExtSourceExpr operand = this.operand;
		if(operand != null) {
			final BadSourceExpr expr = new BadSourceExpr.ExpectedOperator();
			this.operand = new ExtSourceExpr(expr, operand.range.extend(range), EMPTY_SOURCE_MAP);
		} else {
			this.operand = new ExtSourceExpr(token, range, EMPTY_SOURCE_MAP);
		}
		return null;
	}
	@Override @Nullable
	public ExtSourceExpr stringLiteral(FileRange range, StringLiteral token) {
		return visitAtom(range, token);
	}
	@Override @Nullable
	public ExtSourceExpr numberLiteral(FileRange range, NumberLiteral numberLiteral) {
		return visitAtom(range, numberLiteral);
	}
	@Override @Nullable
	public ExtSourceExpr identifier(FileRange range, Identifier simpleName) {
		return visitAtom(range, simpleName);
	}
	@Override @Nullable
	public ExtSourceExpr ellipsis(FileRange range, Ellipsis ellipsis) {
		return visitAtom(range, ellipsis);
	}

	@Override @Nullable
	public ExtSourceExpr operator(FileRange range, OperatorRef token) {
		checkIndentDedent(range, token);
		if(tryCloseParen(range, token)) {
			return null;
		}
		ExtSourceExpr operand = this.operand;
		if(operand != null) {
			// Infix or suffix position
			@Nullable
			final Operator suffixOperator = Operator.fromOp(token.getOp(), Position.SUFFIX);
			if(suffixOperator != null) {
				operand = applyOperatorPrecedenceToStack(operand, suffixOperator); // Apply operator precedence
				final FileRange exprRange = operand.range.extend(range);
				this.operand = operand = new ExtSourceExpr(new UnaryOp(nonNull(suffixOperator), operand.getExpr()), exprRange, operand.getSourceMap());
				return null;
			}
			@Nullable Operator operator = Operator.fromOp(token.getOp(), Position.INFIX);

			if(operator == null) {
				operator = Operator.INVALID;
			}
			// Current operand is the rightmost operand for anything of higher precedence than the operator we just got.
			operand = applyOperatorPrecedenceToStack(operand, operator);
			final ExtSourceExpr operatorExpr = new ExtSourceExpr(token, range, operand.getSourceMap());
			pushPartialOp(new PartialBinaryOp(operator, operand, operatorExpr));
		} else {
			// Prefix position
			@Nullable Operator operator = Operator.fromOp(token.getOp(), Position.PREFIX);
			if(operator == null) {
				operator = Operator.INVALID;
			}
			pushPartialOp(new PartialUnaryOp(range, nonNull(operator)));
		}
		return null;
	}

	public ExtSourceExpr applyOperatorPrecedenceToStack(ExtSourceExpr operand,
			Operator operator) {
		final Precedence prec = operator.getLeftPrecedence();
		final boolean rightAssoc = operator.isRightAssociative();
		while(!this.opStack.isEmpty()
				&& this.opStack.getFirst().getOperator().isParen() == false
				&& !(rightAssoc && this.opStack.getFirst().getOperator() == operator)
				&& (this.opStack.getFirst().getPrecedence().isHigherOrEqual(prec))) {
			final PartialOp op = this.opStack.pop();
			operand = op.rhs(operand);
		}
		return operand;
	}

	private boolean tryCloseParen(FileRange range, OperatorRef opRef) {
		if(opRef.getOp().length() == 1) {
			@Nullable
			final ParenType closeParenTypeMaybe = ParenType.forCloseChar(opRef.getOp().charAt(0));

			if(closeParenTypeMaybe != null) {
				// If there is a trailing comma, semicolon, or newline we can pop it off the stack
				final ParenType closeParenType = nonNull(closeParenTypeMaybe);

				// When the open and close paren are the same, we can't match, so only check for
				// a close paren if we're previously encountered an open paren.
				if(closeParenType.getStartChar() == closeParenType.getEndChar()) {
					boolean found = false;
					for(final PartialOp po : this.opStack) {
						if(po.isOpenParen(closeParenType)) {
							found = true;
							break;
						}
					}
					if(!found)
						return false;
				}

				while(!this.opStack.isEmpty()) {
					final PartialOp po = this.opStack.pop();
					ExtSourceExpr operand = this.operand;
					if(operand == null) {
						operand = new ExtSourceExpr(FileRange.between(po.range, range), new EmptyExpr());
					}
					if(po.isOpenParen(closeParenType)) {
						this.operand = po.closeParen(operand, range);
						break;
					}
					if(po.isParen()) {
						final FileRange newRange = operand.getFileRange().extend(range);
						this.operand = new ExtSourceExpr(new BadSourceExpr.MismatchedCloseParen(closeParenType), newRange, operand.getSourceMap());
						break;
					} else {
						this.operand = po.rhs(operand);
					}
				}
				return true;
			}
		}
		return false;
	}

	@Override
	public @Nullable ExtSourceExpr whitespace(FileRange range, Whitespace ws) {
		return null;
	}
	@Override
	public @Nullable ExtSourceExpr comment(FileRange range, Comment c) {
		return null;
	}

	@Override
	public @Nullable ExtSourceExpr eof(FileRange entireFileRange) {
		this.eof = true;
		ExtSourceExpr operand = this.operand;
		// No operand?  Treat it as an empty expression for now
		if(operand == null) {
			this.operand = operand = new ExtSourceExpr(entireFileRange, new EmptyExpr());
		}
		while(!this.opStack.isEmpty()) {
			this.operand = operand = this.opStack.pop().rhs(nonNull(operand));
		}
		return operand;
	}

	public void reset() {
		this.opStack.clear();
		this.operand = null;
		this.eof = false;
	}

	@Override
	@Nullable
	public ExtSourceExpr badToken(FileRange fileRange, BadToken badToken) {
		// TODO Propagate the error ?
		return null;
	}
}
