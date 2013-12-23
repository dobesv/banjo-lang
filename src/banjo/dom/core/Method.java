package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.Operator;
import banjo.dom.source.Precedence;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.parser.util.AbstractCachedHashCode;
import banjo.parser.util.ListUtil;

public class Method extends AbstractCachedHashCode implements Comparable<Method> {
	private final Key selfName;
	private final Key key;
	private final fj.data.List<MethodParamDecl> args;
	private final CoreExpr guarantee;
	private final CoreExpr body;

	public static final CoreExpr NO_GUARANTEE = MethodParamDecl.NO_ASSERTION;
	public static final Key APPLY_FUNCTION_METHOD_NAME = new Identifier(Operator.CALL.getOp());
	public static final Key LOOKUP_METHOD_NAME = new Identifier(Operator.LOOKUP.getOp());
	public static final Key NO_SELF_NAME = new Identifier("__no_self_name__");
	public static final fj.data.List<MethodParamDecl> NO_ARGS = fj.data.List.<MethodParamDecl>nil();

	public Method(Key selfName, Key key, fj.data.List<MethodParamDecl> args, CoreExpr guarantee, CoreExpr body) {
		super(calcHash(selfName, key, args, guarantee, body));
		this.selfName = selfName;
		this.key = key;
		this.args = args;
		this.guarantee = guarantee;
		this.body = body;
	}

	private static int calcHash(Key selfName, Key key, fj.data.List<MethodParamDecl> args,
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
		final Operator operator = Operator.fromMethodName(this.key.getKeyString());
		if(operator != null && hasSelfName && operator.isInfix() && (operator.isParen() || (!this.args.isEmpty() && this.args.tail().isEmpty()))) {
			sb.append('(');
			if(operator.isParen()) {
				this.selfName.toSource(sb);
				sb.append(operator.getParenType().getStartChar());
				boolean first = true;
				for(final MethodParamDecl arg : this.args) {
					if(first) first = false;
					else sb.append(", ");
					arg.toSource(sb);
				}
				sb.append(operator.getParenType().getEndChar());
			} else if(operator.isRightAssociative()) {
				this.args.head().toSource(sb);
				sb.append(' ');
				operator.toSource(sb);
				sb.append(' ');
				this.selfName.toSource(sb);
			} else {
				this.selfName.toSource(sb);
				sb.append(' ');
				operator.toSource(sb);
				sb.append(' ');
				this.args.head().toSource(sb);
			}
			sb.append(')');
		} else if(operator != null && (operator == Operator.CALL || operator == Operator.BRACKETS)) {
			sb.append(operator.getParenType().getStartChar());
			boolean first = true;
			for(final MethodParamDecl arg : this.args) {
				if(first) first = false;
				else sb.append(", ");
				arg.toSource(sb);
			}
			sb.append(operator.getParenType().getEndChar());
		} else if(operator != null && operator.isPrefix()) {
			sb.append('(');
			operator.toSource(sb);
			this.selfName.toSource(sb);
			sb.append(')');
		} else if(operator != null && operator.isSuffix()) {
			sb.append('(');
			this.selfName.toSource(sb);
			operator.toSource(sb);
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
			if(!this.args.isEmpty()) {
				formalArgListToSource(sb);
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

	public void formalArgListToSource(final StringBuffer sb) {
		sb.append('(');
		boolean first = true;
		for(final MethodParamDecl arg : this.args) {
			if(first) first = false;
			else sb.append(", ");
			arg.toSource(sb);
		}
		sb.append(')');
	}

	public fj.data.List<MethodParamDecl> getArgs() {
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

	/**
	 * True if this method could have been created using the lambda syntax (x,...) -> y
	 */
	public boolean isSimpleApplyMethod() {
		return !hasGuarantee() && !hasSelfName() && this.key.equals(APPLY_FUNCTION_METHOD_NAME);
	}



}