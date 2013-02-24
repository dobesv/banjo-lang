package banjo.parser.ast;

import java.util.LinkedHashMap;

import banjo.parser.util.Token;

public class ObjectLiteral extends Expr {
	
	public static class Field {
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
	
	private final LinkedHashMap<String, Field> fields;

	public ObjectLiteral(LinkedHashMap<String, Field> fields) {
		super();
		this.fields = fields;
	}

	public LinkedHashMap<String, Field> getFields() {
		return fields;
	}
	
	public Field getField(String name) {
		return fields.get(name);
	}
	
	public Expr getFieldValue(String name) {
		return fields.get(name).getValue();
	}
	
}
