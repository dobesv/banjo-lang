package banjo.parser;

import static banjo.parser.util.Check.nonNull;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.ParenType;
import banjo.dom.source.BinaryOp;
import banjo.dom.source.EmptyExpr;
import banjo.dom.source.OffsetSourceExpr;
import banjo.dom.source.Operator;
import banjo.dom.source.Operator.Position;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExpr;
import banjo.dom.source.SourceNode;
import banjo.dom.source.UnaryOp;
import banjo.dom.token.Comment;
import banjo.dom.token.Ellipsis;
import banjo.dom.token.Identifier;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;
import banjo.dom.token.TokenVisitor;
import banjo.dom.token.Whitespace;
import banjo.parser.BanjoParser.ExtSourceExpr;
import banjo.parser.errors.ExpectedOperator;
import banjo.parser.errors.IncorrectIndentation;
import banjo.parser.errors.Problem;
import banjo.parser.errors.UnexpectedCloseParen;
import banjo.parser.errors.UnsupportedBinaryOperator;
import banjo.parser.errors.UnsupportedUnaryOperator;
import banjo.parser.util.FilePos;
import banjo.parser.util.FileRange;
import banjo.parser.util.ListUtil;
import banjo.parser.util.ParserReader;
import banjo.parser.util.Problematic;

/**
 * Change input into an AST.
 */
public class BanjoParser implements TokenVisitor<ExtSourceExpr> {

	private final LinkedList<PartialOp> opStack = new LinkedList<>();
	private @Nullable ExtSourceExpr operand = null;
	private boolean eof;
	private final ArrayList<SourceNode> nodes = new ArrayList<>();

	public static class ExtSourceExpr extends Problematic<SourceExpr> {
		private final FileRange range;
		private final List<SourceNode> nodes;
		private final SourceExpr expr;

		/**
		 * Create an operand info from a SourceExpr plus zero or more ignored
		 * SourceNodes surrounding it.
		 */
		public ExtSourceExpr(FileRange range, List<SourceNode> nodes,
				SourceExpr expr, List<Problem> errors) {
			super(expr, errors);
			this.range = range;
			this.nodes = nodes;
			this.expr = expr;
		}
		public ExtSourceExpr(FileRange range, SourceExpr expr, List<Problem> errors) {
			super(expr, errors);
			this.range = range;
			this.expr = expr;
			this.nodes = nonNull(Collections.<SourceNode>singletonList(expr));
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
		public int offsetIn(FileRange range) {
			return this.range.getStartOffset() - range.getStartOffset();
		}
		public SourceExpr toChildExprIn(FileRange range) {
			final int offset = offsetIn(range);
			if(offset == 0) return getExpr();
			return new OffsetSourceExpr(offset, getExpr());
		}

		@Override
		public ExtSourceExpr withProblems(List<Problem> moreProblems) {
			if(moreProblems.isEmpty())
				return this;
			return new ExtSourceExpr(this.range, this.expr, ListUtil.concat(getProblems(), moreProblems));
		}

	}

	abstract class PartialOp {
		protected final Operator operator;
		protected final FileRange range;
		protected final List<SourceNode> nodes;
		protected final List<Problem> problems;

		public PartialOp(FileRange range, List<SourceNode> children, Operator operator, List<Problem> problems) {
			this.nodes = children;
			this.range = range;
			this.operator = operator;
			this.problems = problems;
		}

		@SafeVarargs
		private final List<SourceNode> concatChildren(List<SourceNode> ... newChildren) {
			return ListUtil.concat(this.nodes,newChildren);
		}

		/**
		 * Create a node for a non-parentheses operator with a right-hand expression.
		 */
		public ExtSourceExpr rhs(ExtSourceExpr rightOperand) {
			List<Problem> errors = rightOperand.getProblems();
			if(rightOperand.getStartColumn() < getStartColumn()) {
				final FileRange indentRange = FileRange.between(this.range, rightOperand.range);
				errors = ListUtil.append(errors, new IncorrectIndentation(indentRange, getStartColumn(), rightOperand.getStartColumn()));
			}
			final FileRange range = this.range.extend(rightOperand.getFileRange());
			return new ExtSourceExpr(range, makeOp(concatChildren(rightOperand.getNodes()), rightOperand.toChildExprIn(range)), ListUtil.concat(errors, this.problems));
		}

		/**
		 * Create a node for a parenthesized expression.
		 */
		public ExtSourceExpr closeParen(ExtSourceExpr operand, FileRange closeParenRange, List<SourceNode> closeParenNodes) {
			List<Problem> errors = operand.getProblems();
			if(operand.getStartColumn() < getStartColumn()) {
				final FileRange indentRange = FileRange.between(this.range, operand.range);
				errors = ListUtil.append(errors, new IncorrectIndentation(indentRange, getStartColumn(), operand.getStartColumn()));
			}
			final FileRange range = this.range.extend(closeParenRange);
			return new ExtSourceExpr(range, makeOp(concatChildren(operand.getNodes(), closeParenNodes), operand.toChildExprIn(range)), ListUtil.concat(errors, this.problems));
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
					operator, ListUtil.concat(operand.getProblems(), operatorExpr.getProblems()));
			this.leftOperand = operand.toChildExprIn(this.range);
		}

		public PartialBinaryOp(Operator operator, ExtSourceExpr operand, List<SourceNode> operatorNodes) {
			super(operand.getFileRange(),
					ListUtil.concat(operand.getNodes(), operatorNodes),
					operator, operand.getProblems());
			this.leftOperand = operand.toChildExprIn(this.range);
		}

		/**
		 * A binary operator where the operator itself has no source representation (newline and indent).
		 */
		public PartialBinaryOp(Operator operator, ExtSourceExpr operand) {
			super(operand.getFileRange(), operand.getNodes(), operator, operand.getProblems());
			this.leftOperand = operand.toChildExprIn(this.range);
		}

		@Override
		public SourceExpr makeOp(List<SourceNode> children, SourceExpr rightOperand) {
			// TODO The second operand must be indented to at least the same position as the first
			//if(this.operator != Operator.NEWLINE && rightStartColumn < getStartColumn()) {
			//	BanjoParser.this.errors.add(new IncorrectIndentation(between(this.opToken, right), getStartColumn(), rightStartColumn, true));
			//}
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
		public PartialUnaryOp(FileRange range, List<SourceNode> children, Operator operator, List<Problem> problems) {
			super(range, children, operator, problems);
		}

		@Override
		SourceExpr makeOp(List<SourceNode> children, SourceExpr rightOperand) {
			// TODO Operand should be at the same level or higher indentation as the unary operator itself
			//			if(operandStartColumn < getStartColumn()) {
			//				BanjoParser.this.errors.add(new IncorrectIndentation(operand.getFileRange(), getStartColumn(), operandStartColumn, true));
			//			}
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
	 * @param scanner Scanner to get tokens from
	 * @return Root of the parsed syntax tree
	 * @throws IOException If the underlying stream throws IOException
	 */
	public ExtSourceExpr parse(ParserReader in) throws IOException {
		reset();
		final BanjoScanner scanner = new BanjoScanner();
		final ExtSourceExpr result = nonNull(scanner.scan(in, this));
		return result.withProblems(scanner.getProblems());
	}

	public ExtSourceExpr parse(String source) throws IOException {
		reset();
		final BanjoScanner scanner = new BanjoScanner();
		return nonNull(scanner.scan(source, this)).withProblems(scanner.getProblems());
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
				pushPartialOp(new PartialBinaryOp(Operator.NEWLINE, operand, consumeNodes()));
			}
		} else if(operand == null &&
				!this.opStack.isEmpty() &&
				this.opStack.getFirst().getEndLine() < line &&
				this.opStack.getFirst().getStartColumn() < column) {
			final int prevColumn = this.opStack.getFirst().getStartColumn();
			final FilePos lineStart = new FilePos(offset-(column-prevColumn), line, prevColumn);
			final FileRange indentRange = new FileRange(lineStart, range.getStart());
			pushPartialOp(new PartialUnaryOp(indentRange, consumeNodes(), Operator.UNARY_NEWLINE_INDENT, Problem.NONE));
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
	private ExtSourceExpr visitLiteral(FileRange range, SourceExpr token) {
		checkIndentDedent(range, token);
		this.nodes.add(token);
		final ExtSourceExpr operand = this.operand;
		final List<Problem> problems = operand == null ? Problem.NONE :
			Problem.one(new ExpectedOperator("Expected operator between "+operand.getExpr()+" and "+token, FileRange.between(operand.getFileRange(), range)));
		this.operand = new ExtSourceExpr(range, consumeNodes(), token, problems);
		return null;
	}
	@Override @Nullable
	public ExtSourceExpr visitStringLiteral(FileRange range, StringLiteral token) {
		return visitLiteral(range, token);
	}
	@Override @Nullable
	public ExtSourceExpr visitNumberLiteral(FileRange range, NumberLiteral numberLiteral) {
		return visitLiteral(range, numberLiteral);
	}
	@Override @Nullable
	public ExtSourceExpr visitIdentifier(FileRange range, Identifier simpleName) {
		return visitLiteral(range, simpleName);
	}
	@Override @Nullable
	public ExtSourceExpr visitEllipsis(FileRange range, Ellipsis ellipsis) {
		return visitLiteral(range, ellipsis);
	}

	@Override @Nullable
	public ExtSourceExpr visitOperator(FileRange range, OperatorRef token) {
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
				this.operand = operand = new ExtSourceExpr(range, new UnaryOp(consumeNodes(), nonNull(suffixOperator), operand.toChildExprIn(range)), operand.getProblems());
				return null;
			}
			@Nullable Operator operator = Operator.fromOp(token.getOp(), Position.INFIX);

			List<Problem> problems = Problem.NONE;
			if(operator == null) {
				if(tryCloseParen(range, token)) {
					return null;
				}
				problems = Problem.one(new UnsupportedBinaryOperator(token.getOp(), range));
				operator = Operator.INVALID;
			}
			// Current operand is the rightmost operand for anything of higher precedence than the operator we just got.
			operand = applyOperatorPrecedenceToStack(operand, operator);
			final ExtSourceExpr operatorExpr = new ExtSourceExpr(range, consumeNodes(), token, problems);
			pushPartialOp(new PartialBinaryOp(operator, operand, operatorExpr));
		} else {
			// Prefix position
			@Nullable Operator operator = Operator.fromOp(token.getOp(), Position.PREFIX);
			List<Problem> problems = Problem.NONE;
			if(operator == null) {
				if(tryCloseParen(range, token)) {
					return null;
				}
				problems = Problem.one(new UnsupportedUnaryOperator(token.getOp(), range));
				operator = Operator.INVALID;
			}
			pushPartialOp(new PartialUnaryOp(range, consumeNodes(), nonNull(operator), problems));
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
						operand = new ExtSourceExpr(FileRange.between(po.range, range), new EmptyExpr(), Problem.NONE);
					}
					if(po.isOpenParen(closeParenType)) {
						this.operand = po.closeParen(operand, range, consumeNodes());
						break;
					}
					if(po.isParen()) {
						final FileRange newRange = operand.getFileRange().extend(range);
						final List<SourceNode> newNodes = ListUtil.concat(operand.nodes, consumeNodes());
						final List<Problem> newProblems = ListUtil.append(operand.getProblems(), new UnexpectedCloseParen(range, closeParenType));
						this.operand = new ExtSourceExpr(newRange, newNodes, operand.getExpr(), newProblems);
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
	public @Nullable ExtSourceExpr visitWhitespace(FileRange range, Whitespace ws) {
		this.nodes.add(ws);
		return null;
	}
	@Override
	public @Nullable ExtSourceExpr visitComment(FileRange range, Comment c) {
		this.nodes.add(c);
		return null;
	}

	@Override
	public @Nullable ExtSourceExpr visitEof(FileRange entireFileRange) {
		this.eof = true;
		ExtSourceExpr operand = this.operand;
		// No operand?  Treat it as an empty expression for now
		if(operand == null) {
			this.operand = operand = new ExtSourceExpr(entireFileRange, new EmptyExpr(consumeNodes()), Problem.NONE);
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
}
