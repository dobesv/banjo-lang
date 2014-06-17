package banjo.dom.core;

import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.Precedence;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.parser.util.AbstractCachedHashCode;
import banjo.parser.util.SourceFileRange;

public class MethodFormalArgument extends AbstractCachedHashCode implements Comparable<MethodFormalArgument> {
	private final Key name;
	private final CoreExpr assertion;
	private final SourceFileRange sourceFileRange;

	public static final CoreExpr NO_ASSERTION = new Identifier(SourceFileRange.SYNTHETIC, "Object");

	public MethodFormalArgument(SourceFileRange sourceFileRange, Key name, CoreExpr assertion) {
		super(name.hashCode() + assertion.hashCode() + sourceFileRange.hashCode());
		this.name = name;
		this.assertion = assertion;
		this.sourceFileRange = sourceFileRange;
	}

	/**
	 * Create a method parameter declaration made of just the name of the parameter,
	 * no assertion.  The source file range is taken from the identifier.
	 * 
	 * @param name  Name of the parameter.
	 */
	public MethodFormalArgument(Key name) {
		this(name.getSourceFileRange(), name, NO_ASSERTION);
	}

	/**
	 * Create a method parameter declaration that may have a different source file
	 * range from its name.  This happens when we're using a pattern to unpack an
	 * object into different parameters.  The object being unpacked is assigned
	 * a temporary name whose file range spans the whole pattern.
	 * 
	 * @param sourceFileRange
	 * @param name
	 */
	public MethodFormalArgument(SourceFileRange sourceFileRange, Key name) {
		this(sourceFileRange, name, NO_ASSERTION);
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
		if (!(obj instanceof MethodFormalArgument))
			return false;
		final MethodFormalArgument other = (MethodFormalArgument) obj;
		if (!this.assertion.equals(other.assertion))
			return false;
		if (!this.name.equals(other.name))
			return false;
		if(!this.sourceFileRange.equals(other.sourceFileRange))
			return false;
		return true;
	}

	@Override
	public int compareTo(@Nullable MethodFormalArgument o) {
		if(this == o)
			return 0;
		if(o == null)
			return -1;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final MethodFormalArgument other = o;
			if(cmp == 0) cmp = this.name.compareTo(other.name);
			if(cmp == 0) cmp = this.assertion.compareTo(other.assertion);
			if(cmp == 0) cmp = this.sourceFileRange.compareTo(other.sourceFileRange);
		}
		return cmp;
	}
	public SourceFileRange getSourceFileRange() {
		return this.sourceFileRange;
	}

}
