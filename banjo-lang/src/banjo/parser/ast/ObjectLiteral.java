package banjo.parser.ast;

import java.util.LinkedHashMap;

import banjo.parser.util.FileRange;


public class ObjectLiteral extends Expr {
	
	private final LinkedHashMap<String, Field> fields;

	public ObjectLiteral(FileRange fileRange, LinkedHashMap<String, Field> fields) {
		super(fileRange);
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
