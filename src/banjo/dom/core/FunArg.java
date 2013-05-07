package banjo.dom.core;

import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExpr;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;

public class FunArg extends AbstractCoreExpr implements CoreExpr {
	private final Key name;
	private final CoreExpr assertion;

	public static final CoreExpr NO_ASSERTION = new Identifier("Object");

	public FunArg(SourceExpr sourceExpr, Key name, CoreExpr assertion) {
		super(sourceExpr, name.hashCode() + assertion.hashCode());
		this.name = name;
		this.assertion = assertion;
	}
	public FunArg(SourceExpr sourceExpr, Key name) {
		this(sourceExpr, name, NO_ASSERTION);
	}

	public Key getName() {
		return this.name;
	}
	public CoreExpr getAssertion() {
		return this.assertion;
	}
	@Override
	public void toSource(StringBuffer sb) {
		sb.append(this.name);
		if(hasContract()) {
			sb.append(": ");
			nonNull(this.assertion).toSource(sb, Precedence.COLON);
		}
	}
	public boolean hasContract() {
		return !NO_ASSERTION.equals(this.assertion);
	}

	@Override
	public Precedence getPrecedence() {
		if(hasContract())
			return Precedence.ASSIGNMENT;
		else
			return Precedence.ATOM;
	}

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		throw new Error();
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

}
