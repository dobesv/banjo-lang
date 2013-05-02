package banjo.analysis;

import banjo.dom.token.Key;

public interface DefRefVisitor {

	void visitRef(DefInfo def, int sourceOffset, Key key);

	void visitDef(DefInfo def);

}
