package banjo.desugar;

import banjo.dom.core.CoreExpr;
import banjo.parser.BanjoParser;
import banjo.parser.BanjoScanner;
import banjo.parser.util.ParserReader;

public class IncrementalUpdater {
	final BanjoParser parser = new BanjoParser();
	final BanjoScanner scanner = new BanjoScanner();
	final BanjoDesugarer desugarer = new BanjoDesugarer();

	static class CoreExprUpdater implements Core

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
