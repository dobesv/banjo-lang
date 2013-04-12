package banjo.dom;

import banjo.parser.util.FileRange;

public interface ExprTransformer {

	/**
	 * Update the file range
	 * 
	 * @param range
	 * @return
	 */
	FileRange transform(FileRange range);
	
	/**
	 * Update a child expression.
	 * 
	 * @param in
	 * @return
	 */
	<T extends Expr> T transform(T in);

	/**
	 * Update the contents of a string, possibly.  If the transformer
	 * is applying a simple edit it might just return a new string
	 * with the edits applied.
	 * 
	 * @param text
	 * @param fileRange
	 * @return
	 */
	String transform(String text, FileRange fileRange);
}
