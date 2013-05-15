package banjo.analysis;

import banjo.dom.token.Key;

public interface DefRefVisitor {

	void visitRef(DefInfo def, int sourceOffset, Key key);

	void visitDef(DefInfo def);

	/**
	 * Called when a ref is found but no matching def could be located.  Thus, it's probably an error.
	 * 
	 * @param sourceOffset
	 * @param key
	 */
	void visitUnresolved(int sourceOffset, Key key);

}
