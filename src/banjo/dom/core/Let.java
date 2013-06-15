package banjo.dom.core;


import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExpr;
import banjo.dom.token.Key;

public class Let extends AbstractCoreExpr implements CoreExpr {
	private final Key name;
	private final CoreExpr value;

	public Let(SourceExpr sourceExpr, Key name, CoreExpr value) {
		this(sourceExpr.getSourceLength(), name, value);
	}

	public Let(int sourceLength, Key name, CoreExpr value) {
		super(sourceLength, name.hashCode() + value.hashCode());
		this.name = name;
		this.value = value;
	}

	public CoreExpr getValue() {
		return this.value;
	}

	public Key getName() {
		return this.name;
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ASSIGNMENT;
	}

	@Override
	public void toSource(StringBuffer sb) {
		this.name.toSource(sb);
		sb.append(" = ");
		this.value.toSource(sb, Precedence.ASSIGNMENT);
	}

	@Override
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.let(this);
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (!(obj instanceof Let))
			return false;
		final Let other = (Let) obj;
		if (!this.name.equals(other.name))
			return false;
		if (!this.value.equals(other.value))
			return false;
		return true;
	}

}
