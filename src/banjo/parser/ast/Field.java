package banjo.parser.ast;

public class Field {
	private final Key key;
	private final Expr value;
	public Field(Key key, Expr valueExpr) {
		this.key = key;
		this.value = valueExpr;
	}
	public Expr getValue() {
		return value;
	}
	public Key getKey() {
		return key;
	}
	
}