package banjo.parser.ast;

import java.util.LinkedHashMap;


public class ObjectLiteral extends Expr {
	
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
