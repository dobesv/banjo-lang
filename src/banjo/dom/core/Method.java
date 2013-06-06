package banjo.dom.core;

import java.util.Iterator;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.Operator;
import banjo.dom.source.Precedence;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;

public class Method {
	private final Key key;
	private final CoreExpr implementation;
	private final int offsetInObject;
	private final int sourceLength;
	private final boolean include;

	public static final Key DEFAULT_SELF_NAME = new Identifier("_self");

	public Method(int sourceLength, int offsetInObject, Key key, CoreExpr implementation, boolean include) {
		super();
		this.sourceLength = sourceLength;
		this.offsetInObject = offsetInObject;
		this.key = key;
		this.implementation = implementation;
		this.include = include;
		if(!FunctionLiteral.isFunctionLiteral(implementation)) throw new IllegalArgumentException();
	}

	public Key getKey() {
		return this.key;
	}

	public CoreExpr getImplementation() {
		return this.implementation;
	}

	public void toSource(final StringBuffer sb) {
		this.implementation.acceptVisitor(new BaseCoreExprVisitor<Void>() {
			@Override
			@Nullable
			public Void functionLiteral(FunctionLiteral implementation) {
				final Iterator<FunArg> it = implementation.getArgs().iterator();
				if(it.hasNext()) {
					final FunArg selfArg = it.next();
					if(!selfArg.getName().equals(DEFAULT_SELF_NAME)) {
						selfArg.toSource(sb);
						sb.append('.');
					}
				}
				Method.this.key.toSource(sb);
				if(it.hasNext()) {
					sb.append('(');
					boolean first = true;
					while(it.hasNext()) {
						final FunArg arg = it.next();
						if(first) first = false;
						else sb.append(", ");
						arg.toSource(sb);
					}
					sb.append(')');
				}
				sb.append(Method.this.include ? Operator.PAIR_INCLUDE.getOp() : Operator.PAIR.getOp());
				sb.append(' ');
				implementation.getBody().toSource(sb, Precedence.COLON);
				return null;
			}

			@Override
			@Nullable
			public Void fallback(CoreExpr unsupported) {
				// Shouldn't happen, really ...
				Method.this.key.toSource(sb);
				sb.append(':');
				sb.append(' ');
				unsupported.toSource(sb, Precedence.COLON);
				return null;
			}
		});
	}

	public int getOffsetInObject() {
		return this.offsetInObject;
	}

	public int getSourceLength() {
		return this.sourceLength;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + this.implementation.hashCode();
		result = prime * result + this.key.hashCode();
		result = prime * result + this.offsetInObject;
		result = prime * result + this.sourceLength;
		return result;
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof Method))
			return false;
		final Method other = (Method) obj;
		if (!this.implementation.equals(other.implementation))
			return false;
		if (!this.key.equals(other.key))
			return false;
		if (this.offsetInObject != other.offsetInObject)
			return false;
		if (this.sourceLength != other.sourceLength)
			return false;
		return true;
	}

	public boolean isInclude() {
		return this.include;
	}



}