package banjo.parser;

import static banjo.parser.util.Check.nonNull;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.ParenType;
import banjo.dom.source.BadSourceExpr;
import banjo.dom.source.BinaryOp;
import banjo.dom.source.EmptyExpr;
import banjo.dom.source.Operator;
import banjo.dom.source.Operator.Position;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExpr;
import banjo.dom.source.SourceNode;
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
import banjo.parser.util.ListUtil;
import banjo.parser.util.ParserReader;
import banjo.parser.util.SourceMap;

/**
 * Change input into an AST.
 */
public class BanjoParser implements TokenVisitor<ExtSourceExpr> {

	private final LinkedList<PartialOp> opStack = new LinkedList<>();
	private @Nullable ExtSourceExpr operand = null;
	private boolean eof;
	private final ArrayList<SourceNode> nodes = new ArrayList<>();
	static final SourceMap<SourceExpr> EMPTY_SOURCE_MAP = new SourceMap<SourceExpr>();

	public static class ExtSourceExpr {
		private final FileRange range;
		private final List<SourceNode> nodes;
		private final SourceMap<SourceExpr> sourceMaps;
		private final SourceExpr expr;

		/**
		 * Create one from a SourceExpr with a file range and source maps for all its children
		 */
		public ExtSourceExpr(FileRange range, List<SourceNode> nodes, SourceMap<SourceExpr> sourceMaps, SourceExpr expr) {
			this.range = range;
			this.nodes = nodes;
			this.expr = expr;
			this.sourceMaps = sourceMaps.insert(expr, range.toOffsetLength());
		}
		/**
		 * Create one from an atom.  The expression should be made of just one SourceNode and have no
		 * children.
		 */
		public ExtSourceExpr(FileRange range, SourceExpr expr) {
			this.range = range;
			this.expr = expr;
			this.nodes = nonNull(Collections.<SourceNode>singletonList(expr));
			this.sourceMaps = EMPTY_SOURCE_MAP.insert(expr, range.toOffsetLength());
		}
		public FileRange getFileRange() {
			return this.range;
		}
		public List<SourceNode> getNodes() {
			return this.nodes;
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
			return this.range+": "+this.expr+" "+this.nodes;
		}
		public SourceMap<SourceExpr> getSourceMaps() {
			return this.sourceMaps;
		}
	}

	abstract class PartialOp {
		protected final Operator operator;
		protected final FileRange range;
		protected final List<SourceNode> nodes;
		protected final SourceMap<SourceExpr> sourceMaps;
		protected final ExtSourceExpr operatorExpr;

		/**
		 * 
		 * @param range
		 * @param nodes SourceNode instances that make up the source expression.  They must be continuous (no gaps)
		 * @param operator
		 * @param problems
		 */
		public PartialOp(FileRange range, List<SourceNode> nodes, SourceMap<SourceExpr> sourceMaps, Operator operator, ExtSourceExpr operatorExpr) {
			this.nodes = nodes;
			this.sourceMaps = sourceMaps;
			this.range = range;
			this.operator = operator;
			this.operatorExpr = operatorExpr;
		}

		@SafeVarargs
		private final List<SourceNode> concatChildren(List<SourceNode> ... newChildren) {
			return ListUtil.concat(this.nodes,newChildren);
		}

		/**
		 * Create a node for a non-parentheses operator with a right-hand expression.
		 */
		public ExtSourceExpr rhs(ExtSourceExpr rightOperand) {
			final FileRange range = this.range.extend(rightOperand.getFileRange());
			final List<SourceNode> allNodes = concatChildren(rightOperand.getNodes());
			return _rhs(rightOperand, range, allNodes);
		}

		/**
		 * Create a node for a parenthesized expression.
		 */
		public ExtSourceExpr closeParen(ExtSourceExpr rightOperand, FileRange closeParenRange, List<SourceNode> closeParenNodes) {
			final FileRange range = this.range.extend(closeParenRange);
			final List<SourceNode> allNodes = concatChildren(rightOperand.getNodes(), closeParenNodes);
			return _rhs(rightOperand, range, allNodes);
		}

		private ExtSourceExpr _rhs(ExtSourceExpr rightOperand, final FileRange range, final List<SourceNode> allNodes) {
			if(this.operator == Operator.INVALID) {
				return new ExtSourceExpr(range, allNodes, this.sourceMaps.union(rightOperand.getSourceMaps()), new BadSourceExpr.UnsupportedOperator(this.operatorExpr.expr.toSource()));
			}
			if(rightOperand.getStartColumn() < getStartColumn()) {
				rightOperand = new ExtSourceExpr(rightOperand.range, rightOperand.nodes, rightOperand.getSourceMaps(), new BadSourceExpr.IncorrectIndentation(rightOperand.getStartColumn(), getStartColumn()));
			}
			final SourceExpr newExpr = makeOp(allNodes, rightOperand.getExpr());
			final SourceMap<SourceExpr> sourceMaps = this.sourceMaps.union(rightOperand.getSourceMaps()).insert(newExpr, range.toOffsetLength());
			return new ExtSourceExpr(range, allNodes, sourceMaps, newExpr);
		}

		/**
		 * Create a node for a non-parentheses operator with a right-hand expression.
		 */
		abstract SourceExpr makeOp(List<SourceNode> children, SourceExpr sourceExpr);

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

		public int getEndLine() {
			return this.range.getEndLine();
		}

		public boolean isNewline() {
			switch(this.operator) {
			case UNARY_NEWLINE_INDENT:
			case NEWLINE:
				return true;
			default:
				return false;
			}
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
					ListUtil.concat(operand.getNodes(), operatorExpr.getNodes()),
					operand.getSourceMaps().union(operatorExpr.getSourceMaps()),
					operator, operatorExpr);
			this.leftOperand = operand.getExpr();
		}

		public PartialBinaryOp(Operator operator, ExtSourceExpr operand, FileRange operatorRange, List<SourceNode> operatorNodes) {
			this(operator, operand, new ExtSourceExpr(operatorRange, operatorNodes, EMPTY_SOURCE_MAP, new OperatorRef(operator.getOp())));
		}

		@Override
		public SourceExpr makeOp(List<SourceNode> children, SourceExpr rightOperand) {
			return new BinaryOp(children, this.operator, this.leftOperand, rightOperand);
		}

		@Override
		public String toString() {
			return "(" + getOperand().toSource()+" "+this.operator.getOp() +" _)";
		}
		public SourceExpr getOperand() {
			return this.leftOperand;
		}

		public List<SourceNode> getNodes() {
			return this.nodes;
		}

		public FileRange getFileRange() {
			return this.range;
		}
	}

	class PartialUnaryOp extends PartialOp {

		public PartialUnaryOp(FileRange range, List<SourceNode> children, Operator operator) {
			super(range, children, EMPTY_SOURCE_MAP, operator, new ExtSourceExpr(range, children, EMPTY_SOURCE_MAP, new OperatorRef(operator.getOp())));
		}

		@Override
		SourceExpr makeOp(List<SourceNode> children, SourceExpr rightOperand) {
			if(this.operator == Operator.NEGATE && rightOperand instanceof NumberLiteral) {
				return ((NumberLiteral)rightOperand).negate();
			}
			if(this.operator == Operator.PLUS && rightOperand instanceof NumberLiteral) {
				return rightOperand;
			}
			return new UnaryOp(children, this.operator, rightOperand);
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
	 * Check for indent/dedent prior to the given token.  May consume the contents of nodes with the assumption it is
	 * whitespace/comments only.
	 * 
	 * @param range The range of the token
	 * @param token The token we just encountered (should not be whitespace or a comment)
	 */
	private void checkIndentDedent(FileRange range, SourceExpr token) {
		final int line = range.getStartLine();
		final int column = range.getStartColumn();
		final int offset = range.getStartOffset();
		ExtSourceExpr operand = this.operand;
		if(operand != null && line > operand.getEndLine()) {
			// Now if we get a de-dent we have to move up the operator stack
			while(!this.opStack.isEmpty()
					&& this.opStack.getFirst().getStartColumn() >= column
					&& this.opStack.getFirst().isParen() == false) {
				final PartialOp op = this.opStack.pop();
				this.operand = operand = op.rhs(operand);
			}

			// If we de-dented back to an exact match on the column of an enclosing
			// expression, insert a newline operator
			if(operand.getStartColumn() == column) {
				pushPartialOp(new PartialBinaryOp(Operator.NEWLINE, operand, FileRange.between(operand.getFileRange(), range), consumeNodes()));
			}
			//		} else if(operand == null &&
			//				!this.opStack.isEmpty() &&
			//				this.opStack.getFirst().getEndLine() < line &&
			//				this.opStack.getFirst().getStartColumn() < column) {
			//			final int prevColumn = this.opStack.getFirst().getStartColumn();
			//			final FilePos lineStart = new FilePos(offset-(column-prevColumn), line, prevColumn);
			//			final FileRange indentRange = new FileRange(lineStart, range.getStart());
			//			pushPartialOp(new PartialUnaryOp(indentRange, consumeNodes(), Operator.UNARY_NEWLINE_INDENT));
		}
	}

	public void pushPartialOp(PartialOp partialOp) {
		this.opStack.push(partialOp);
		this.operand = null;
	}

	public List<SourceNode> consumeNodes() {
		final List<SourceNode> nodes =
				(this.nodes.isEmpty() ? nonNull(Collections.<SourceNode>emptyList()) :
					this.nodes.size() == 1 ? nonNull(Collections.singletonList(this.nodes.get(0))) :
						new ArrayList<>(this.nodes));
		this.nodes.clear();
		return nodes;
	}

	@Nullable
	private ExtSourceExpr visitAtom(FileRange range, SourceExpr token) {
		checkIndentDedent(range, token);
		this.nodes.add(token);
		final ExtSourceExpr operand = this.operand;
		if(operand != null) {
			final BadSourceExpr expr = new BadSourceExpr.ExpectedOperator();
			this.operand = new ExtSourceExpr(operand.range.extend(range), ListUtil.concat(operand.nodes, consumeNodes()), EMPTY_SOURCE_MAP, expr);
		} else {
			this.operand = new ExtSourceExpr(range, consumeNodes(), EMPTY_SOURCE_MAP, token);
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
		this.nodes.add(token);
		ExtSourceExpr operand = this.operand;
		if(operand != null) {
			// Infix or suffix position
			@Nullable
			final Operator suffixOperator = Operator.fromOp(token.getOp(), Position.SUFFIX);
			if(suffixOperator != null) {
				this.nodes.addAll(0, operand.getNodes()); // Insert operand nodes as prefix
				operand = applyOperatorPrecedenceToStack(operand, suffixOperator); // Apply operator precedence
				final List<SourceNode> allNodes = consumeNodes();
				final FileRange exprRange = operand.range.extend(range);
				this.operand = operand = new ExtSourceExpr(exprRange, allNodes, operand.getSourceMaps(), new UnaryOp(allNodes, nonNull(suffixOperator), operand.getExpr()));
				return null;
			}
			@Nullable Operator operator = Operator.fromOp(token.getOp(), Position.INFIX);

			if(operator == null) {
				if(tryCloseParen(range, token)) {
					return null;
				}
				operator = Operator.INVALID;
			}
			// Current operand is the rightmost operand for anything of higher precedence than the operator we just got.
			operand = applyOperatorPrecedenceToStack(operand, operator);
			final ExtSourceExpr operatorExpr = new ExtSourceExpr(range, consumeNodes(), operand.getSourceMaps(), token);
			pushPartialOp(new PartialBinaryOp(operator, operand, operatorExpr));
		} else {
			// Prefix position
			@Nullable Operator operator = Operator.fromOp(token.getOp(), Position.PREFIX);
			if(operator == null) {
				if(tryCloseParen(range, token)) {
					return null;
				}
				operator = Operator.INVALID;
			}
			pushPartialOp(new PartialUnaryOp(range, consumeNodes(), nonNull(operator)));
		}
		return null;
	}

	public ExtSourceExpr applyOperatorPrecedenceToStack(ExtSourceExpr operand,
			Operator operator) {
		final Precedence prec = operator.getPrecedence();
		final boolean rightAssoc = operator.isRightAssociative();
		while(!this.opStack.isEmpty()
				&& this.opStack.getFirst().getOperator().isParen() == false
				&& (rightAssoc ? this.opStack.getFirst().getPrecedence().isHigherThan(prec)
						: this.opStack.getFirst().getPrecedence().isHigherOrEqual(prec))) {
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

				while(!this.opStack.isEmpty()) {
					final PartialOp po = this.opStack.pop();
					ExtSourceExpr operand = this.operand;
					if(operand == null) {
						operand = new ExtSourceExpr(FileRange.between(po.range, range), new EmptyExpr());
					}
					if(po.isOpenParen(closeParenType)) {
						this.operand = po.closeParen(operand, range, consumeNodes());
						break;
					}
					if(po.isParen()) {
						final FileRange newRange = operand.getFileRange().extend(range);
						final List<SourceNode> newNodes = ListUtil.concat(operand.nodes, consumeNodes());
						this.operand = new ExtSourceExpr(newRange, newNodes, operand.getSourceMaps(), new BadSourceExpr.MismatchedCloseParen(closeParenType));
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
			this.operand = operand = new ExtSourceExpr(entireFileRange, new EmptyExpr(consumeNodes()));
		}
		while(!this.opStack.isEmpty()) {
			this.operand = operand = this.opStack.pop().rhs(nonNull(operand));
		}
		return operand;
	}

	public void reset() {
		this.nodes.clear();
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
