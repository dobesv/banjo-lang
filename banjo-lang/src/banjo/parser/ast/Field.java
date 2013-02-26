package banjo.parser.ast;

import banjo.parser.util.Token;

public class Field {
	final Token identifier;
	final Expr value;
	public Field(Token identifier, Expr value) {
		super();
		this.identifier = identifier;
		this.value = value;
	}
	public Token getIdentifier() {
		return identifier;
	}
	public Expr getValue() {
		return value;
	}
}