package banjo.idesupport;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

import banjo.desugar.BanjoDesugarer;
import banjo.dom.BadExpr;
import banjo.dom.Call;
import banjo.dom.CoreExpr;
import banjo.dom.CoreExprVisitor;
import banjo.dom.Expr;
import banjo.dom.ExprList;
import banjo.dom.ExprTransformer;
import banjo.dom.Field;
import banjo.dom.FieldRef;
import banjo.dom.FunctionLiteral;
import banjo.dom.HasFileRange;
import banjo.dom.Identifier;
import banjo.dom.Let;
import banjo.dom.ListLiteral;
import banjo.dom.NumberLiteral;
import banjo.dom.ObjectLiteral;
import banjo.dom.OperatorRef;
import banjo.dom.SetLiteral;
import banjo.dom.SourceExpr;
import banjo.dom.StringLiteral;
import banjo.parser.BanjoParser;
import banjo.parser.BanjoScanner;
import banjo.parser.errors.BanjoParseException;
import banjo.parser.errors.UnexpectedIOExceptionError;
import banjo.parser.util.FilePos;
import banjo.parser.util.FileRange;
import banjo.parser.util.ParserReader;
import fj.data.Option;

public class IncrementalUpdater {

	private final class ExprTransformerImplementation implements
			ExprTransformer {
		private final int editEndOffset;
		private final int lineDelta;
		private final String newSourceCode;
		private final int editStartLine;
		private final int editStartColumn;
		private final int offsetDelta;
		private final int columnDelta;
		private final int editStartOffset;
		final CoreExprVisitor<Expr> visitor = new CoreExprVisitor<Expr>() {

			@Override
			public CoreExpr visitStringLiteral(StringLiteral stringLiteral) {
				if(touchesEdit(stringLiteral)) {
					return reparse(stringLiteral);
				}
				return transform(stringLiteral);
			}

			@Override
			public CoreExpr visitNumberLiteral(NumberLiteral numberLiteral) {
				if(touchesEdit(numberLiteral)) {
					return reparse(numberLiteral);
				}
				return transform(numberLiteral);
			}

			@Override
			public CoreExpr visitIdentifier(Identifier identifier) {
				if(touchesEdit(identifier)) {
					return reparse(identifier);
				}
				return transform(identifier);
			}

			@Override
			public CoreExpr visitOperator(OperatorRef operatorRef) {
				if(touchesEdit(operatorRef)) {
					return reparse(operatorRef);
				}
				return transform(operatorRef);
			}

			@Override
			public CoreExpr visitCall(Call call) {
				if(editTouchesStartOrEndOfNode(call.getCallee()) ||
						editTouchesStartOrEndOfChild(call.getArguments())) {
					return reparse(call);
				}
				return transform(call);
			}

			@Override
			public CoreExpr visitExprList(ExprList exprList) {
				if(editTouchesStartOrEndOfChild(exprList.getElements())) {
					return reparse(exprList);
				}
				return transform(exprList);
			}

			@Override
			public CoreExpr visitFieldRef(FieldRef fieldRef) {
				if(editTouchesStartOrEndOfNode(fieldRef.getBase()) ||
						editTouchesStartOrEndOfNode(fieldRef.getKey())) {
					return reparse(fieldRef);
				}
				return transform(fieldRef);
			}

			@Override
			public CoreExpr visitFunctionLiteral(FunctionLiteral functionLiteral) {
				if(editTouchesStartOrEndOfNode(functionLiteral.getBody()) ||
						editTouchesStartOrEndOfNode(functionLiteral.getContract()) ||
						editTouchesStartOrEndOfNode(functionLiteral.getSelfName()) ||
						editTouchesStartOrEndOfChild(functionLiteral.getArgs())) {
					return reparse(functionLiteral);
				}
				return transform(functionLiteral);
			}

			@Override
			public CoreExpr visitObjectLiteral(ObjectLiteral objectLiteral) {
				for(Field f : objectLiteral.getFields().values()) {
					if(editTouchesStartOrEndOfNode(f.getKey()) ||
							editTouchesStartOrEndOfNode(f.getValue())) {
						return reparse(objectLiteral);
					}
				}
				return transform(objectLiteral);
			}

			@Override
			public CoreExpr visitLet(Let let) {
				if(editTouchesStartOrEndOfNode(let.getName()) ||
						editTouchesStartOrEndOfNode(let.getValue())) {
					return reparse(let);
				}
				return transform(let);
			}

			@Override
			public CoreExpr visitListLiteral(ListLiteral listLiteral) {
				if(editTouchesStartOrEndOfChild(listLiteral.getElements())) {
					return reparse(listLiteral);
				}
				return transform(listLiteral);
			}

			@Override
			public CoreExpr visitSetLiteral(SetLiteral setLiteral) {
				if(editTouchesStartOrEndOfChild(setLiteral.getElements())) {
					return reparse(setLiteral);
				}
				return transform(setLiteral);
			}


			@Override
			public CoreExpr visitBadExpr(BadExpr badExpr) {
				return transform(badExpr);
			}
		};

		private ExprTransformerImplementation(int endOffset, int lineDelta,
				String newSourceCode, int editStartLine, int editStartColumn,
				int offsetDelta, int columnDelta, int editOffset) {
			this.editEndOffset = endOffset;
			this.lineDelta = lineDelta;
			this.newSourceCode = newSourceCode;
			this.editStartLine = editStartLine;
			this.editStartColumn = editStartColumn;
			this.offsetDelta = offsetDelta;
			this.columnDelta = columnDelta;
			this.editStartOffset = editOffset;
		}

		CoreExpr transform(CoreExpr e) {
			// TODO - this should only be necessary for the root node ...
			if(editTouchesStartOrEndOfNode(e)) {
				return reparse(e);
			}
			return (CoreExpr) e.transform(this);
		}

		CoreExpr reparse(CoreExpr e) {
			int nodeStartOffset = adjustOffset(e.getFileRange().getStartOffset(), false);
			int nodeEndOffset = adjustOffset(e.getFileRange().getEndOffset(), true);
			ParserReader in = ParserReader.fromSubstring(e.getFileRange().getFilename(), newSourceCode, nodeStartOffset, nodeEndOffset);
			try {
				SourceExpr node = parser.parse(in, errors);
				if(node == null) throw new NullPointerException();
				return desugarer.desugar(node, errors);
			} catch (IOException e1) {
				throw new UnexpectedIOExceptionError(e1);
			}
		}

		<T extends HasFileRange> boolean editTouchesStartOrEndOfNode(Option<T> optExpr) {
			if(optExpr.isSome()) {
				HasFileRange expr = optExpr.some();
				if(expr == null) throw new NullPointerException();
				return editTouchesStartOrEndOfNode(expr);
			}
			return false;
		}

		/**
		 * @return true if the edited range affects either the first character or the character
		 *              following the last character of the given expression.
		 */
		private boolean editTouchesStartOrEndOfNode(HasFileRange child) {
			FileRange range = child.getFileRange();
			
			return (editStartOffset <= range.getStartOffset() && editEndOffset >= range.getStartOffset()) ||
				   (editStartOffset <= range.getEndOffset() && editEndOffset >= range.getEndOffset());
		}

		private <T extends HasFileRange> boolean editTouchesStartOrEndOfChild(Collection<T> children) {
			for(HasFileRange child : children) {
				if(child == null) throw new NullPointerException();
				if(editTouchesStartOrEndOfNode(child))
					return true;
			}
			return false;
		}

		/**
		 * @return True if the edit affects characters contained in the node's range
		 */
		private boolean touchesEdit(CoreExpr child) {
			FileRange range = child.getFileRange();
			return editEndOffset >= range.getStartOffset() && editStartOffset <= range.getEndOffset();
		}

		@Override
		public String transform(String text, FileRange fileRange) {
			return text;
		}

		@SuppressWarnings("unchecked")
		@Override
		public <T extends Expr> T transform(T in) {
			Expr result = ((CoreExpr)in).acceptVisitor(visitor);
			if(result == null) throw new NullPointerException();
			return (T) result;
		}

		// TODO This line number adjustment isn't right - if the offset is inside the edited range we need a more fine-grained adjustment
		private int adjustLine(int offset, int line, boolean inclusive) {
			return (inclusive ? offset >= editStartOffset : offset > editStartOffset) ? line + lineDelta : line;
		}

		private int adjustColumn(int offset, int line, int column, boolean inclusive) {
			return (inclusive ? offset >= editStartOffset : offset > editStartOffset) &&
					line == editStartLine && 
					(inclusive ? column >= editStartColumn : column > editStartColumn) ? column + columnDelta : column;
		}

		private int adjustOffset(int offset, boolean inclusive) {
			return (inclusive ? offset >= editStartOffset : offset > editStartOffset) ? offset + offsetDelta : offset;
		}

		private FilePos adjustFilePos(FilePos pos, boolean inclusive) {
			int offset = pos.getOffset();
			return editStartOffset < offset ? pos : 
				new FilePos(adjustOffset(offset, inclusive),
						    adjustLine(offset, pos.getLine(), inclusive),
						    adjustColumn(offset, pos.getLine(), pos.getColumn(), inclusive));
		}

		@Override
		public FileRange transform(FileRange range) {
			// Easiest - this range comes before the offset of the change, so it can be left untouched
			if(editStartOffset > range.getEndOffset()) {
				return range;
			}
			
			// Create a new range by adjusting the offsets / lines
			return new FileRange(range.getFilename(),
					adjustFilePos(range.getStart(), false),
					adjustFilePos(range.getEnd(), true));
		}
	}

	public IncrementalUpdater() {
	}

	final BanjoParser parser = new BanjoParser();
	final BanjoScanner scanner = new BanjoScanner();
	final BanjoDesugarer desugarer = new BanjoDesugarer();
	final ArrayList<BanjoParseException> errors = new ArrayList<>();
	
	/**
	 * Return an AST representing how things should be after applying the given edit.  This does
	 * its best to re-use as much of the original AST as possible.
	 * 
	 * 
	 * @param expr Original AST
	 * @param editOffset Offset of the change
	 * @param editLength Number of characters to remove/replace
	 * @param replacement New text to insert
	 * @return
	 */
	public CoreExpr applyEdit(CoreExpr expr, final int editOffset, final int editLength, final int editStartLine, final int editStartColumn, final String replacement, final String newSourceCode) {
		/*
		 * Different cases while transforming:
		 * 
		 * 1. Expr touching the change, but with a single child expr that touches the change, 
		 *    and that child completely covers the change: transform that child, adjust range
		 * 2. Expr containing the change, but with multiple child exprs touching the change, or
		 *    the change extends beyond the edge of a single child: re-parse from source range
		 * 3. String literals, numbers, and identifiers where the change can applied and the token is still
		 *    valid: apply change, adjust range
		 * 4. Expr before the change: leave alone
		 * 5. Expr after the change: adjust range
		 */
		// SourceExpr newContentType = parser.parse(replacement);
		final int endOffset = editOffset + editLength;
		
		int columnDeltaTemp=0;
		int lineDeltaTemp=0;
		final int offsetDelta = replacement.length() - editLength;
		for(int i=0; i < replacement.length(); i++) {
			char ch = replacement.charAt(i);
			if(ch == '\n') {
				lineDeltaTemp++;
				columnDeltaTemp = -editStartColumn;
			} else {
				columnDeltaTemp ++;
			}
		}
		final int columnDelta = columnDeltaTemp;
		final int lineDelta = lineDeltaTemp;
		
		final ExprTransformerImplementation transformer = new ExprTransformerImplementation(endOffset, lineDelta, newSourceCode,
				editStartLine, editStartColumn, offsetDelta, columnDelta,
				editOffset);
		if(transformer.editTouchesStartOrEndOfNode(expr)) {
			return transformer.reparse(expr);
		}
		return transformer.transform(expr);
	}
}
