package banjo.dom.core;

import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.Precedence;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.parser.util.AbstractCachedHashCode;

public class MethodParamDecl extends AbstractCachedHashCode implements Comparable<MethodParamDecl> {
	private final Key name;
	private final CoreExpr assertion;

	public static final CoreExpr NO_ASSERTION = new Identifier("Object");

	public MethodParamDecl(Key name, CoreExpr assertion) {
		super(name.hashCode() + assertion.hashCode());
		this.name = name;
		this.assertion = assertion;
	}
	public MethodParamDecl(Key name) {
		this(name, NO_ASSERTION);
	}

	public Key getName() {
		return this.name;
	}
	public CoreExpr getAssertion() {
		return this.assertion;
	}

	@SuppressWarnings("null")
	public String toSource() {
		final StringBuffer sb = new StringBuffer();
		toSource(sb);
		return sb.toString();
	}

	@Override
	public String toString() {
		return toSource();
	}
	public void toSource(StringBuffer sb) {
		sb.append(this.name);
		if(hasAssertion()) {
			sb.append(": ");
			nonNull(this.assertion).toSource(sb, Precedence.COLON);
		}
	}
	public boolean hasAssertion() {
		return !NO_ASSERTION.equals(this.assertion);
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (!(obj instanceof MethodParamDecl))
			return false;
		final MethodParamDecl other = (MethodParamDecl) obj;
		if (!this.assertion.equals(other.assertion))
			return false;
		if (!this.name.equals(other.name))
			return false;
		return true;
	}

	@Override
	public int compareTo(MethodParamDecl o) {
		if(this == o)
			return 0;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final MethodParamDecl other = o;
			if(cmp == 0) cmp = this.name.compareTo(other.name);
			if(cmp == 0) cmp = this.assertion.compareTo(other.assertion);
		}
		return cmp;
	}

}
