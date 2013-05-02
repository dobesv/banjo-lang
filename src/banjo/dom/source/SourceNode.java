package banjo.dom.source;

import java.util.List;

import fj.data.Option;


public interface SourceNode {

	/**
	 * Append normalized source to the given StringBuffer.
	 */
	void toSource(StringBuffer sb);

	/**
	 * Length of the token in the source file.
	 * 
	 * TODO What if it's not parsed from a source file but created programmatically?
	 */
	int getSourceLength();

	/**
	 * Find the offset into this source expr to the given child expression.
	 * 
	 * This does a recursive scan of the AST rooted at the target node.  It may be
	 * an expensive operation.
	 * 
	 * Returns NOT_A_CHILD if the child could not be found.
	 */
	Option<Integer> offsetToChild(SourceExpr sourceExpr);

	/**
	 * If this is a composite source node, return the child source nodes.  Otherwise
	 * return an empty list.
	 */
	List<SourceNode> getSourceNodes();
}
