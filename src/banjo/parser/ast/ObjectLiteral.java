package banjo.parser.ast;

import java.util.Collections;
import java.util.Map;

import banjo.parser.util.FileRange;


public class ObjectLiteral extends Expr {
	
	private final Map<String, Field> fields;

	public ObjectLiteral(FileRange fileRange, Map<String, Field> fields) {
		super(fileRange);
		this.fields = Collections.unmodifiableMap(fields);
	}

	public Map<String, Field> getFields() {
		return fields;
	}
	
	public Field getField(String name) {
		return fields.get(name);
	}
	
	public Expr getFieldValue(String name) {
		return fields.get(name).getValue();
	}
	
	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}
	
	@Override
	public void toSource(StringBuffer sb) {
		sb.append('{');
		boolean first = true;
		for(Field f : fields.values()) {
			if(first) first = false;
			else sb.append(", ");
			sb.append(f.getIdentifier());
			sb.append(": ");
			f.getValue().toSource(sb, Precedence.ASSIGNMENT);
		}
		sb.append('}');
	}
}
