package banjo.dom.token;

import banjo.dom.core.CoreExpr;
import banjo.dom.source.Atom;


public interface Key extends Atom, CoreExpr {
	public String getKeyString();
}
