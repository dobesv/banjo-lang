package banjo.dom;

import fj.data.Option;

public interface Key extends Atom, CoreExpr {
	public String getKeyString();
	
	public static final Option<Key> NONE = Option.none();
}
