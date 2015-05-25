package banjo.expr.source;




public interface SourceNode {

	/**
	 * Append normalized source to the given StringBuffer.
	 */
	void toSource(StringBuffer sb);

}
