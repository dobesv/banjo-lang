package banjo.dom.core;

import banjo.dom.ExprTransformer;
import banjo.dom.token.Key;

public class Field {
	private final Key key;
	private final CoreExpr value;
	public Field(Key key, CoreExpr value) {
		this.key = key;
		this.value = value;
	}
	public CoreExpr getValue() {
		return value;
	}
	public Key getKey() {
		return key;
	}
	public Field transform(ExprTransformer transformer) {
		Key newKey = transformer.transform(key);
		CoreExpr newValue = transformer.transform(value);
		if(newKey == key && newValue == value)
			return this;
		return new Field(newKey, newValue);
	}
	
}