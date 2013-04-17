package banjo.analysis;

import banjo.dom.Key;

public interface DefRefVisitor {

	void visitRef(DefInfo def, Key key);

	void visitDef(DefInfo def);

}
