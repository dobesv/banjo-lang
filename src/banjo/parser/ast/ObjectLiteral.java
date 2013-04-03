package banjo.parser.ast;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import banjo.parser.BanjoParser;
import banjo.parser.util.FileRange;


public class ObjectLiteral extends BaseExpr {
	
	private final Map<String, Field> fields;

	public ObjectLiteral(FileRange fileRange, Map<String, Field> fields) {
		super(fileRange);
		this.fields = Collections.unmodifiableMap(fields);
	}

	public ObjectLiteral(FileRange fileRange, Field ... fields) {
		this(fileRange, makeFieldMap(Arrays.asList(fields)));
	}

	private static Map<String, Field> makeFieldMap(List<Field> fields) {
		LinkedHashMap<String,Field> fieldMap = new LinkedHashMap<>(fields.size());
		for(Field f : fields) {
			fieldMap.put(f.getKey().getKeyString(), f);
		}
		return fieldMap;
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
			f.getKey().toSource(sb);
			sb.append(": ");
			f.getValue().toSource(sb, Precedence.ASSIGNMENT);
		}
		sb.append('}');
	}

	public static StringBuffer maybeQuoteKey(String identifier, StringBuffer sb) {
		for(int i=0; i < identifier.length(); i++) {
			int cp = identifier.codePointAt(i);
			if(cp > Character.MAX_VALUE) i++; // Actually a pair of characters
			boolean ok = i==0 ? BanjoParser.isIdentifierStart(cp):BanjoParser.isIdentifierPart(cp);
			if(!ok) {
				return StringLiteral.toSource(identifier, sb);
			}
		}
		sb.append(identifier);
		return sb;
	}
}
