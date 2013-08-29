package banjo.dom.core;

import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.Precedence;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.parser.util.AbstractCachedHashCode;

public class FunArg extends AbstractCachedHashCode implements Comparable<FunArg> {
	private final Key name;
	private final CoreExpr assertion;

	public static final CoreExpr NO_ASSERTION = new Identifier("Object");

	public FunArg(Key name, CoreExpr assertion) {
		super(name.hashCode() + assertion.hashCode());
		this.name = name;
		this.assertion = assertion;
	}
	public FunArg(Key name) {
		this(name, NO_ASSERTION);
	}

	public Key getName() {
		return this.name;
	}
	public CoreExpr getAssertion() {
		return this.assertion;
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
		if (!(obj instanceof FunArg))
			return false;
		final FunArg other = (FunArg) obj;
		if (!this.assertion.equals(other.assertion))
			return false;
		if (!this.name.equals(other.name))
			return false;
		return true;
	}

	@Override
	public int compareTo(FunArg o) {
		if(this == o)
			return 0;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final FunArg other = o;
			if(cmp == 0) cmp = this.name.compareTo(other.name);
			if(cmp == 0) cmp = this.assertion.compareTo(other.assertion);
		}
		return cmp;
	}

}
