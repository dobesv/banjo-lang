package banjo.desugar;

import static banjo.parser.util.Check.nonNull;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Expr;
import banjo.dom.ExprTransformer;
import banjo.dom.core.BadExpr;
import banjo.dom.core.Call;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.CoreExprVisitor;
import banjo.dom.core.ExprList;
import banjo.dom.core.Field;
import banjo.dom.core.FieldRef;
import banjo.dom.core.FunArg;
import banjo.dom.core.FunctionLiteral;
import banjo.dom.core.Let;
import banjo.dom.core.ListLiteral;
import banjo.dom.core.ObjectLiteral;
import banjo.dom.core.SetLiteral;
import banjo.dom.source.SourceExpr;
import banjo.dom.source.SourceNode;
import banjo.dom.token.Identifier;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;
import banjo.parser.BanjoParser;
import banjo.parser.BanjoScanner;
import banjo.parser.errors.BanjoParseException;
import banjo.parser.errors.UnexpectedIOExceptionError;
import banjo.parser.util.FileRange;
import banjo.parser.util.OffsetLength;
import banjo.parser.util.ParserReader;
import fj.data.Option;

public class IncrementalUpdater {


	@Nullable
	private CoreExpr parentExpr;
	private int parentExprOffset;

	CoreExpr transform(CoreExpr e) {
		final OffsetLength range = sourceRange(e);

		// If the edit is completely outside the range of that node, leave it untouched
		if(this.editEndOffset < range.getOffset() || this.editStartOffset > range.getEnd())
			return e;

		// If the edit touches the start/end of the range, reparse
		if(editTouchesStartOrEndOfRange(range)) {
			return reparse(e);
		}

		final CoreExpr oldParentExpr = this.parentExpr;
		final int oldParentExprOffset = this.parentExprOffset;
		this.parentExpr = e;
		this.parentExprOffset = range.getOffset();

		// Otherwise check whether we need to reparse because the edit spans multiple child nodes
		for(final java.lang.reflect.Field f : e.getClass().getDeclaredFields()) {
			final Object child = f.get(e);
			if(child instanceof CoreExpr) {
				final SourceExpr sourceExpr = ((CoreExpr)child).getSourceExpr();
				for(final SourceNode node : sourceExpr.getSourceNodes()) {
					if(editTouchesStartOrEndOfRange(sourceRange(node))) {
						return reparse(e);
					}
				}
			}
			if(child instanceof SourceNode) {

			}
		}


		for(final java.lang.reflect.Field f : e.getClass().getDeclaredFields()) {
			final Object child = f.get(e);

		}

		this.parentExpr = oldParentExpr;
		this.parentExprOffset = oldParentExprOffset;
		return result;
	}

	private int sourceOffset(final CoreExpr childExpr) {
		final CoreExpr parentExpr = this.parentExpr;
		if(childExpr == parentExpr) return this.parentExprOffset;
		if(parentExpr == null) return 0; // Must be the root node
		final Option<Integer> offsetFromParent = parentExpr.getSourceExpr().offsetToChild(childExpr.getSourceExpr());
		if(offsetFromParent.isNone()) throw new IllegalStateException("Looking for offset to node that isn't a child of the current node");
		return this.parentExprOffset + offsetFromParent.orSome(0).intValue();
	}
	private OffsetLength sourceRange(final CoreExpr childExpr) {
		return new OffsetLength(sourceOffset(childExpr), childExpr.getSourceExpr().getSourceLength());
	}

	CoreExpr reparse(CoreExpr e) {
		final OffsetLength range = sourceRange(e);
		final int nodeStartOffset = adjustOffset(range.getOffset(), false);
		final int nodeEndOffset = adjustOffset(range.getEnd(), true);
		final ParserReader in = ParserReader.fromSubstring("", this.newSourceCode, nodeStartOffset, nodeEndOffset);
		try {
			final SourceExpr node = IncrementalUpdater.this.parser.parse(in, IncrementalUpdater.this.errors);
			if(node == null) throw new NullPointerException();
			return IncrementalUpdater.this.desugarer.desugar(node, IncrementalUpdater.this.errors);
		} catch (final IOException e1) {
			throw new UnexpectedIOExceptionError(e1);
		}
	}

	/**
	 * @return true if the edited range affects either the first character or the character
	 *              following the last character of the given expression.
	 */
	private boolean editTouchesStartOrEndOfNode(@Nullable CoreExpr expr) {
		if(expr == null) return false;
		final OffsetLength range = sourceRange(expr);

		return editTouchesStartOrEndOfRange(range);
	}

	public boolean editTouchesStartOrEndOfRange(final OffsetLength range) {
		return (this.editStartOffset <= range.getOffset() && this.editEndOffset >= range.getOffset()) ||
				(this.editStartOffset <= range.getEnd() && this.editEndOffset >= range.getEnd());
	}

	private <T extends CoreExpr> boolean editTouchesStartOrEndOfChild(Collection<T> children) {
		for(final CoreExpr child : children) {
			if(editTouchesStartOrEndOfNode(nonNull(child)))
				return true;
		}
		return false;
	}

	/**
	 * @return True if the edit affects characters contained in the node's range
	 */
	private boolean touchesEdit(CoreExpr child) {
		final OffsetLength range = sourceRange(child);
		return this.editEndOffset >= range.getOffset() && this.editStartOffset <= range.getEnd();
	}

	@Override
	public String transform(String text, FileRange fileRange) {
		return text;
	}

	@SuppressWarnings("unchecked")
	@Override
	public <T extends Expr> T transform(T in) {
		final Expr result = ((CoreExpr)in).acceptVisitor(this.visitor);
		if(result == null) throw new NullPointerException();
		return (T) result;
	}
	private int adjustOffset(int offset, boolean inclusive) {
		return (inclusive ? offset >= this.editStartOffset : offset > this.editStartOffset) ? offset + this.offsetDelta : offset;
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
	 *    valid: apply change
	 */

	/*
	 * Adjusting the parse tree is not a big deal.  But adjusted the desugared nodes is trickier.  The
	 * root node will always have to updated since at least one child will be updated.  So some smarts
	 * is needed to walk down the tree and do minimal damage
	 */
	// SourceExpr newContentType = parser.parse(replacement);
	final int endOffset = editOffset + editLength;

	int columnDeltaTemp=0;
	int lineDeltaTemp=0;
	final int offsetDelta = replacement.length() - editLength;
	for(int i=0; i < replacement.length(); i++) {
		final char ch = replacement.charAt(i);
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

public CoreExpr applyEdit(CoreExpr ast, int offset, int length, String replacement, String newSourceCode) {
	final ParserReader temp = ParserReader.fromSubstring("", newSourceCode, offset, offset+length);
	return applyEdit(ast, offset, length, temp.getCurrentLineNumber(), temp.getCurrentColumnNumber(), replacement, newSourceCode);
}
}
