package banjo.dom.core;

import static banjo.parser.util.Check.nonNull;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExpr;
import banjo.dom.token.StringLiteral;
import banjo.parser.BanjoScanner;


public class ObjectLiteral extends AbstractCoreExpr implements CoreExpr {

	private final Map<String, Field> fields;

	public ObjectLiteral(SourceExpr sourceExpr, Map<String, Field> fields) {
		super(sourceExpr, fields.hashCode());
		this.fields = nonNull(Collections.unmodifiableMap(fields));
	}

	@SafeVarargs
	public ObjectLiteral(SourceExpr sourceExpr, Field ... fields) {
		this(sourceExpr, makeFieldMap(nonNull(Arrays.asList(fields))));
	}

	private static Map<String, Field> makeFieldMap(List<Field> fields) {
		final LinkedHashMap<String,Field> fieldMap = new LinkedHashMap<>(fields.size());
		for(final Field f : fields) {
			fieldMap.put(f.getKey().getKeyString(), f);
		}
		return fieldMap;
	}

	public Map<String, Field> getFields() {
		return this.fields;
	}

	public @Nullable Field getField(String name) {
		return this.fields.get(name);
	}

	public CoreExpr getFieldValue(String name) {
		return this.fields.get(name).getValue();
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append('{');
		boolean first = true;
		for(final Field f : this.fields.values()) {
			if(first) first = false;
			else sb.append(", ");
			f.toSource(sb);
		}
		sb.append('}');
	}

	public static StringBuffer maybeQuoteKey(String identifier, StringBuffer sb) {
		for(int i=0; i < identifier.length(); i++) {
			final int cp = identifier.codePointAt(i);
			if(cp > Character.MAX_VALUE) i++; // Actually a pair of characters
			final boolean ok = i==0 ? BanjoScanner.isIdentifierStart(cp):BanjoScanner.isIdentifierPart(cp);
			if(!ok) {
				return StringLiteral.toSource(identifier, sb);
			}
		}
		sb.append(identifier);
		return sb;
	}


	@Override
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.objectLiteral(this);
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (!(obj instanceof ObjectLiteral))
			return false;
		final ObjectLiteral other = (ObjectLiteral) obj;
		if (!this.fields.equals(other.fields))
			return false;
		return true;
	}

}
