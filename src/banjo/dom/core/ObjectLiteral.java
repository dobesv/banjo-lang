package banjo.dom.core;

import static banjo.parser.util.Check.nonNull;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Expr;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExpr;
import banjo.dom.token.StringLiteral;
import banjo.parser.BanjoScanner;
import banjo.parser.util.FileRange;


public class ObjectLiteral extends AbstractCoreExpr implements CoreExpr {
	
	private final Map<String, Field> fields;

	public ObjectLiteral(SourceExpr sourceExpr, Map<String, Field> fields) {
		super(sourceExpr);
		this.fields = nonNull(Collections.unmodifiableMap(fields));
	}

	public ObjectLiteral(SourceExpr sourceExpr, Field ... fields) {
		this(sourceExpr, makeFieldMap(nonNull(Arrays.asList(fields))));
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
	
	public @Nullable Field getField(String name) {
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
			boolean ok = i==0 ? BanjoScanner.isIdentifierStart(cp):BanjoScanner.isIdentifierPart(cp);
			if(!ok) {
				return StringLiteral.toSource(identifier, sb);
			}
		}
		sb.append(identifier);
		return sb;
	}


	@Override
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.visitObjectLiteral(this);
	}	
}
