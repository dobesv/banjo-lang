package banjo.dom.core;

import java.util.Iterator;
import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.Operator;
import banjo.dom.source.Operator.Position;
import banjo.dom.source.Precedence;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.parser.util.AbstractCachedHashCode;
import banjo.parser.util.ListUtil;

public class Method extends AbstractCachedHashCode implements Comparable<Method> {
	private final Key selfName;
	private final Key key;
	private final List<MethodParamDecl> args;
	private final CoreExpr guarantee;
	private final CoreExpr body;

	public static final CoreExpr NO_GUARANTEE = MethodParamDecl.NO_ASSERTION;
	public static final Key APPLY_FUNCTION_METHOD_NAME = new Identifier(Operator.CALL.getOp());
	public static final Key LOOKUP_METHOD_NAME = new Identifier(Operator.LOOKUP.getOp());
	public static final Key NO_SELF_NAME = new Identifier("");

	public Method(Key selfName, Key key, List<MethodParamDecl> args, CoreExpr guarantee, CoreExpr body) {
		super(calcHash(selfName, key, args, guarantee, body));
		this.selfName = selfName;
		this.key = key;
		this.args = args;
		this.guarantee = guarantee;
		this.body = body;
	}

	private static int calcHash(Key selfName, Key key, List<MethodParamDecl> args,
			CoreExpr guarantee, CoreExpr body) {
		final int prime = 31;
		int result = 1;
		result = prime * result + selfName.hashCode();
		result = prime * result + key.hashCode();
		result = prime * result + args.hashCode();
		result = prime * result + guarantee.hashCode();
		result = prime * result + body.hashCode();
		return result;
	}

	public Key getKey() {
		return this.key;
	}


	@SuppressWarnings("null")
	@Override
	public String toString() {
		final StringBuffer sb = new StringBuffer();
		toSource(sb);
		return sb.toString();
	}

	public void toSource(final StringBuffer sb) {
		// Check for brackets, apply

		final boolean hasSelfName = this.hasSelfName();
		final Operator infixOperator = Operator.fromOp(this.key.getKeyString(), Position.INFIX);
		final Operator prefixOperator = hasSelfName && this.args.size() == 0 ? Operator.fromOp(this.key.getKeyString(), Position.PREFIX) : null;
		final Operator suffixOperator = hasSelfName && this.args.size() == 0 ? Operator.fromOp(this.key.getKeyString(), Position.SUFFIX) : null;
		if(infixOperator != null && hasSelfName && (infixOperator.isParen() || this.args.size() == 1)) {
			sb.append('(');
			this.selfName.toSource(sb);
			if(infixOperator.isParen()) {
				sb.append(infixOperator.getParenType().getStartChar());
				boolean first = true;
				for(final MethodParamDecl arg : this.args) {
					if(first) first = false;
					else sb.append(", ");
					arg.toSource(sb);
				}
				sb.append(infixOperator.getParenType().getEndChar());
			} else {
				sb.append(' ');
				infixOperator.toSource(sb);
				sb.append(' ');
				this.args.get(0).toSource(sb);
			}
			sb.append(')');
		} else if(infixOperator != null && (infixOperator == Operator.CALL || infixOperator == Operator.BRACKETS)) {
			sb.append(infixOperator.getParenType().getStartChar());
			boolean first = true;
			for(final MethodParamDecl arg : this.args) {
				if(first) first = false;
				else sb.append(", ");
				arg.toSource(sb);
			}
			sb.append(infixOperator.getParenType().getEndChar());
		} else if(prefixOperator != null) {
			sb.append('(');
			prefixOperator.toSource(sb);
			this.selfName.toSource(sb);
			sb.append(')');
		} else if(suffixOperator != null) {
			sb.append('(');
			this.selfName.toSource(sb);
			suffixOperator.toSource(sb);
			sb.append(')');
		} else {
			if(hasSelfName) {
				this.selfName.toSource(sb);
				sb.append('.');
			} else if(this.key.equals(this.body) && this.args.isEmpty() && this.guarantee.equals(NO_GUARANTEE)) {
				this.key.toSource(sb);
				return;
			}
			this.key.toSource(sb);
			final Iterator<MethodParamDecl> it = this.args.iterator();
			if(it.hasNext()) {
				sb.append('(');
				boolean first = true;
				while(it.hasNext()) {
					final MethodParamDecl arg = it.next();
					if(first) first = false;
					else sb.append(", ");
					arg.toSource(sb);
				}
				sb.append(')');
			}
		}
		if(hasGuarantee()) {
			sb.append(' ');
			Operator.COLON.toSource(sb);
			sb.append(' ');
			this.guarantee.toSource(sb, Precedence.COLON);
		}
		sb.append(' ');
		Operator.ASSIGNMENT.toSource(sb);
		sb.append(' ');
		this.body.toSource(sb, Precedence.COLON);
	}

	public List<MethodParamDecl> getArgs() {
		return this.args;
	}

	public CoreExpr getGuarantee() {
		return this.guarantee;
	}

	public CoreExpr getBody() {
		return this.body;
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (!(obj instanceof Method))
			return false;
		final Method other = (Method) obj;
		if (!this.args.equals(other.args))
			return false;
		if (!this.body.equals(other.body))
			return false;
		if (!this.guarantee.equals(other.guarantee))
			return false;
		if (!this.key.equals(other.key))
			return false;
		if (!this.selfName.equals(other.selfName))
			return false;
		return true;
	}

	@Override
	public int compareTo(Method other) {
		int cmp = this.key.compareTo(other.key);
		if(cmp == 0) cmp = ListUtil.compare(this.args, other.args);
		if(cmp == 0) cmp = this.guarantee.compareTo(other.guarantee);
		if(cmp == 0) cmp = this.selfName.compareTo(other.selfName);
		if(cmp == 0) cmp = this.body.compareTo(other.body);
		return cmp;
	}

	public boolean hasSelfName() {
		return !this.selfName.equals(NO_SELF_NAME);
	}

	public Key getSelfName() {
		return this.selfName;
	}

	public boolean hasGuarantee() {
		return !this.guarantee.equals(NO_GUARANTEE);
	}



}