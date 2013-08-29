package banjo.dom.core;

import java.util.Iterator;
import java.util.List;

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
	private final List<FunArg> args;
	private final CoreExpr guarantee;
	private final CoreExpr body;

	public static final CoreExpr NO_GUARANTEE = FunArg.NO_ASSERTION;
	public static final Key APPLY_FUNCTION_METHOD_NAME = new Identifier(Operator.CALL.getOp());
	public static final Key LOOKUP_METHOD_NAME = new Identifier(Operator.LOOKUP.getOp());
	public static final Key NO_SELF_NAME = new Identifier("");

	public Method(Key selfName, Key key, List<FunArg> args, CoreExpr guarantee, CoreExpr body) {
		super(calcHash(selfName, key, args, guarantee, body));
		this.selfName = selfName;
		this.key = key;
		this.args = args;
		this.guarantee = guarantee;
		this.body = body;
	}

	private static int calcHash(Key selfName, Key key, List<FunArg> args,
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



	public void toSource(final StringBuffer sb) {
		if(!this.selfName.equals(NO_SELF_NAME)) {
			this.selfName.toSource(sb);
			sb.append('.');
		} else if(this.key.equals(this.body) && this.args.isEmpty() && this.guarantee.equals(NO_GUARANTEE)) {
			this.key.toSource(sb);
			return;
		}
		final boolean applyMethod = this.key.getKeyString().equals(APPLY_FUNCTION_METHOD_NAME.getKeyString());
		if(!applyMethod) {
			this.key.toSource(sb);
		}
		final Iterator<FunArg> it = this.args.iterator();
		if(it.hasNext() || applyMethod) {
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
		sb.append(' ');
		sb.append(Operator.ASSIGNMENT.getOp());
		sb.append(' ');
		this.body.toSource(sb, Precedence.COLON);
	}

	public List<FunArg> getArgs() {
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
		if (obj == null)
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
		return 0;
	}



}