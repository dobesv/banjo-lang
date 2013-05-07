package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExpr;
import banjo.dom.token.Key;

public class Field {
	private final Key key;
	private final CoreExpr value;
	private final SourceExpr sourceExpr;
	private final int offsetInObject;

	public Field(SourceExpr sourceExpr, int offsetInObject, Key key, CoreExpr value) {
		this.sourceExpr = sourceExpr;
		this.key = key;
		this.value = value;
		this.offsetInObject = offsetInObject;
	}
	public CoreExpr getValue() {
		return this.value;
	}
	public Key getKey() {
		return this.key;
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (!(obj instanceof Field))
			return false;
		final Field other = (Field) obj;
		if (!this.key.equals(other.key))
			return false;
		if (!this.value.equals(other.value))
			return false;
		if (!this.sourceExpr.equals(other.sourceExpr))
			return false;
		if (this.offsetInObject != other.offsetInObject)
			return false;
		return true;
	}

	@Override
	public int hashCode() {
		return (this.key.hashCode() + this.value.hashCode()) ^ this.sourceExpr.hashCode();
	}

	public void toSource(StringBuffer sb) {
		this.key.toSource(sb);
		sb.append(": ");
		this.value.toSource(sb, Precedence.ASSIGNMENT);
	}

	public SourceExpr getSourceExpr() {
		return this.sourceExpr;
	}
	public int getOffsetInObject() {
		return this.offsetInObject;
	}

}