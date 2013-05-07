package banjo.dom.source;

import java.util.List;


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
	 * If this is a composite source node, return the child source nodes.  Otherwise
	 * return an empty list.
	 */
	List<SourceNode> getSourceNodes();

}
