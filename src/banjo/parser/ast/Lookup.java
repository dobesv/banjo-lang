package banjo.parser.ast;

import banjo.parser.util.FileRange;

public class Lookup extends BaseExpr {
	private final Expr map;
	private final Expr key;
	
	public Lookup(FileRange range, Expr collection, Expr key) {
		super(range);
		this.map = collection;
		this.key = key;
	}
	public Expr getMap() {
		return map;
	}
	public Expr getKey() {
		return key;
	}
	@Override
	public Precedence getPrecedence() {
		return Precedence.SUFFIX;
	}
	
	@Override
	public void toSource(StringBuffer sb) {
		map.toSource(sb, Precedence.SUFFIX);
		sb.append('[');
		key.toSource(sb);
		sb.append(']');
	}
	
}
