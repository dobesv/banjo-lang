package banjo.parser;

import java.io.IOException;
import java.util.Collection;
import java.util.LinkedList;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Atom;
import banjo.dom.BinaryOp;
import banjo.dom.Comment;
import banjo.dom.Consts;
import banjo.dom.Ellipsis;
import banjo.dom.HasFileRange;
import banjo.dom.Identifier;
import banjo.dom.NumberLiteral;
import banjo.dom.Operator;
import banjo.dom.Operator.Position;
import banjo.dom.OperatorRef;
import banjo.dom.ParenType;
import banjo.dom.Precedence;
import banjo.dom.SourceExpr;
import banjo.dom.StringLiteral;
import banjo.dom.Token;
import banjo.dom.TokenVisitor;
import banjo.dom.UnaryOp;
import banjo.dom.UnitRef;
import banjo.dom.Whitespace;
import banjo.parser.errors.BanjoParseException;
import banjo.parser.errors.ExpectedExpression;
import banjo.parser.errors.ExpectedOperator;
import banjo.parser.errors.IncorrectIndentation;
import banjo.parser.errors.UnexpectedCloseParen;
import banjo.parser.errors.UnsupportedBinaryOperator;
import banjo.parser.errors.UnsupportedUnaryOperator;
import banjo.parser.util.FilePos;
import banjo.parser.util.FileRange;
import banjo.parser.util.ParserReader;
import fj.data.Option;

/**
 * Change input into an AST.
 */
public class BanjoParser implements TokenVisitor<SourceExpr> {

	private final LinkedList<BanjoParseException> errors = new LinkedList<>();
	private LinkedList<PartialOp> opStack = new LinkedList<>();
	@Nullable private SourceExpr operand = null;
	private boolean eof;


	abstract class PartialOp {
		protected final Operator operator;
		protected final OperatorRef opToken;
		public PartialOp(Operator operator, OperatorRef opToken) {
			this.opToken = opToken;
			this.operator = operator;
		}
		
		public abstract SourceExpr makeOp(OperatorRef closeParen);

		abstract SourceExpr makeOp(SourceExpr operand, Option<OperatorRef> closeParen);
		abstract int getStartColumn();
		public Operator getOperator() {
			return operator;
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
			return opToken.getFileRange().getEndLine();
		}
		public FileRange getOpRange() {
			return opToken.getFileRange();
		}
		
	}
	class PartialBinaryOp extends PartialOp {
		private final SourceExpr left;
		
		public PartialBinaryOp(Operator operator, OperatorRef opToken, SourceExpr operand) {
			super(operator, opToken);
			this.left = operand;
		}

		/**
		 * Make an op when a close paren is found with no expression inside.
		 */
		@Override
		public SourceExpr makeOp(OperatorRef closeParen) {
			// Operand followed by empty parens
			if(operator == Operator.CALL) {
				return new UnaryOp(Operator.UNARY_CALL, opToken, left, Option.some(closeParen));
			} else {
				errors.add(new ExpectedExpression(new FileRange(left.getFileRange(), getOpRange())));
				return left;
			}
		}
		
		@Override
		public SourceExpr makeOp(SourceExpr right, Option<OperatorRef> closeParen) {
			// The second operand must be indented to at least the same position as the first
			if(!isParen() && 
					operator != Operator.NEWLINE &&
					right.getStartColumn() < getStartColumn()) {
				errors.add(new IncorrectIndentation(right.getFileRange(), getStartColumn(), true));
			}
			return new BinaryOp(operator, getOperand(), opToken, right, closeParen);
		}
		
		public int getStartColumn() {
			return left.getStartColumn();
		}
		@Override
		public String toString() {
			return "(" + getOperand().toSource()+" "+operator.getOp() +" _)";
		}
		public SourceExpr getOperand() {
			return left;
		}
	}
	
	class PartialUnaryOp extends PartialOp {
		public PartialUnaryOp(Operator operator, OperatorRef opToken) {
			super(operator, opToken);
		}
		
		@Override
		public SourceExpr makeOp(OperatorRef closeParen) {
			// Operator followed by close paren ...
			if(operator.isParen()) {
				return new UnitRef(new FileRange(getOpRange(), closeParen.getFileRange()), operator.getParenType());
			} else {
				errors.add(new ExpectedExpression(closeParen.getFileRange()));
				return opToken;
			}
		}
		public SourceExpr makeOp(SourceExpr operand, Option<OperatorRef> closeParen) {
			if(operator == Operator.NEWLINE_INDENT)
				return operand;
			
			if(!isParen() && operand.getStartColumn() < getStartColumn()) {
				// Operand should be at the same level or higher indentation as the unary operator itself
				errors.add(new IncorrectIndentation(operand.getFileRange(), getStartColumn(), true));
			}
			return new UnaryOp(operator, opToken, operand, closeParen);
		}
		public int getStartColumn() {
			return getOpRange().getStart().getColumn();
		}
		@Override
		public String toString() {
			return "("+operator.getOp()+" _)";
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
	public @Nullable SourceExpr parse(ParserReader in) throws IOException {
		return new BanjoScanner().scan(in, this, errors);
	}
	
	public @Nullable SourceExpr parse(String source) throws IOException {
		return new BanjoScanner().scan(source, this, errors);
	}

	void dropTrailingCommaOrSemicolon() {
		if(operand == null &&
		   !opStack.isEmpty() && 
		   (opStack.getFirst() instanceof PartialBinaryOp) &&
		   isListSeparator(((PartialBinaryOp) opStack.getFirst()).getOperator())) {
			operand = ((PartialBinaryOp) opStack.pop()).getOperand();
		}
	}

	private void pushPartialBinaryOp(Operator operator, OperatorRef opToken) {
		// Current operand is the rightmost operand for anything of higher precedence than the operator we just got.
		Precedence prec = operator.getPrecedence();
		boolean rightAssoc = operator.isRightAssociative();
		SourceExpr operand = this.operand;
		if(operand == null) throw new NullPointerException();
		while(!opStack.isEmpty() 
				&& opStack.getFirst().getOperator().isParen() == false
				&& (rightAssoc ? opStack.getFirst().getPrecedence().isHigherThan(prec)
						       : opStack.getFirst().getPrecedence().isHigherOrEqual(prec))) {
			operand = opStack.pop().makeOp(operand, Option.<OperatorRef>none());
		}
		opStack.push(new PartialBinaryOp(operator, opToken, operand));
		this.operand = null;
	}

	private FileRange between(HasFileRange a, HasFileRange b) {
		return new FileRange(a.getFileRange().getFilename(), a.getFileRange().getEnd(), b.getFileRange().getStart());
	}

	public Collection<BanjoParseException> getErrors() {
		return errors;
	}

	/**
	 * @return true iff we have reached the end of the input
	 */
	public boolean reachedEof() {
		return eof;
	}
	
	private boolean isListSeparator(Operator operator) {
		switch(operator) {
		case COMMA:
		case SEMICOLON:
		case NEWLINE:
			return true;
		default:
			return false;
		}
	}

	private void checkIndentDedent(SourceExpr token) {
		FilePos tokenStartPos = token.getFileRange().getStart();
		int line = tokenStartPos .getLine();
		int column = tokenStartPos.getColumn();
		SourceExpr operand = this.operand;
		if(operand != null && line > operand.getFileRange().getEndLine()) {
			// Now if we get a de-dent we have to move up the operator stack
			while(!opStack.isEmpty() 
					&& opStack.getFirst().getStartColumn() >= column
					&& opStack.getFirst().isParen() == false) {
				this.operand = operand = opStack.pop().makeOp(operand, Consts.OPTOKEN_NONE);
			}
			
			// If we de-dented back to an exact match on the column of an enclosing
			// expression, insert a newline operator
			if(operand.getStartColumn() == column) {
				opStack.push(new PartialBinaryOp(Operator.NEWLINE, new OperatorRef(token.getFileRange(), "\\n"), operand));
				this.operand = null;
			}
		}
		
		if(operand == null && 
				!opStack.isEmpty() &&
				opStack.getFirst().getEndLine() < line &&
				opStack.getFirst().getStartColumn() < column) {
			opStack.push(new PartialUnaryOp(Operator.NEWLINE_INDENT, new OperatorRef(token.getFileRange(), "\\n\\t")));
		}
	}
	@Nullable
	private SourceExpr visitLiteral(SourceExpr token) {
		checkIndentDedent(token);
		final SourceExpr operand = this.operand;
		if(operand != null) {
			errors.add(new ExpectedOperator(between(operand, token)));
		}
		this.operand = token;
		return null;
	}
	@Override @Nullable
	public SourceExpr visitStringLiteral(StringLiteral token) {
		return visitLiteral(token);
	}
	@Override @Nullable
	public SourceExpr visitNumberLiteral(NumberLiteral numberLiteral) {
		return visitLiteral(numberLiteral);
	}
	@Override @Nullable
	public SourceExpr visitIdentifier(Identifier simpleName) {
		return visitLiteral(simpleName);
	}
	@Override @Nullable
	public SourceExpr visitEllipsis(Ellipsis ellipsis) {
		return visitLiteral(ellipsis);
	}
	@Override @Nullable
	public SourceExpr visitUnit(UnitRef unit) {
		return visitLiteral(unit);
	}
	@Override
	public SourceExpr visitOperator(OperatorRef token) {
		checkIndentDedent(token);
		final SourceExpr operand = this.operand;
		if(operand != null) {
			// Infix or suffix position
			Option<Operator> suffixOperator = Operator.fromOp(token.getOp(), Position.SUFFIX);
			if(suffixOperator.isSome()) {
				this.operand = new UnaryOp(suffixOperator.some(), token, operand, Option.<OperatorRef>none());
				return token;
			}
			Option<Operator> operator = Operator.fromOp(token.getOp(), Position.INFIX);
			if(operator.isNone()) {
				if(tryCloseParen(token)) {
					return token;
				}
				errors.add(new UnsupportedBinaryOperator(token.getOp(), token.getFileRange()));
				operator = Option.some(Operator.INVALID);
			}
			pushPartialBinaryOp(operator.some(), token);
		} else {
			// Prefix position
			Option<Operator> operator = Operator.fromOp(token.getOp(), Position.PREFIX);
			if(operator.isNone()) {
				if(tryCloseParen(token)) {
					return token;
				}
				errors.add(new UnsupportedUnaryOperator(token.getOp(), token.getFileRange()));
				operator = Option.some(Operator.INVALID);
			}
			opStack.push(new PartialUnaryOp(operator.some(), token));
		}
		return token;
	}

	private boolean tryCloseParen(OperatorRef opRef) {
		if(opRef.getOp().length() == 1) {
			Option<ParenType> closeParenTypeMaybe = ParenType.forCloseChar(opRef.getOp().charAt(0));
			if(closeParenTypeMaybe.isSome()) {
				// If there is a trailing comma, semicolon, or newline we can pop it off the stack
				dropTrailingCommaOrSemicolon();
				final ParenType closeParenType = closeParenTypeMaybe.some();
				
				while(!opStack.isEmpty()) {
					PartialOp po = opStack.pop();
					final SourceExpr operand = this.operand;
					if(operand == null)
						this.operand = po.makeOp(opRef);
					else
						this.operand = po.makeOp(operand, Option.some(opRef));
					if(po.isOpenParen(closeParenType)) {
						break;
					} else if(po.isParen()) {
						errors.add(new UnexpectedCloseParen(opRef.getFileRange(), closeParenType));
					}
				}
				return true;
			}		
		}
		return false;
	}
	@Override
	public @Nullable SourceExpr visitWhitespace(Whitespace ws) {
		return null;
	}
	@Override
	public @Nullable SourceExpr visitComment(Comment c) {
		return null;
	}
	
	@Override
	public @Nullable SourceExpr visitEof() {
		eof = true;
		SourceExpr operand = this.operand;
		if(operand == null) {
			// Parse failed
			return null;
		}
		while(!opStack.isEmpty()) {
			this.operand = operand = opStack.pop().makeOp(operand, Consts.OPTOKEN_NONE);
		}
		return operand;
	}
}
