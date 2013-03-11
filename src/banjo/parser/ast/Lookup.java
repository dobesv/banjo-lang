package banjo.parser.ast;

import banjo.parser.util.FileRange;

public class Lookup extends Expr {
	private final Expr collection;
	private final Expr key;
	
	public Lookup(FileRange range, Expr collection, Expr key) {
		super(range);
		this.collection = collection;
		this.key = key;
	}
	public Expr getCollection() {
		return collection;
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
		collection.toSource(sb, Precedence.SUFFIX);
		sb.append('[');
		key.toSource(sb);
		sb.append(']');
	}
	
}
