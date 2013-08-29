package banjo.dom.core;

import static banjo.parser.util.Check.nonNull;

import java.util.Arrays;
import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Expr;
import banjo.dom.source.Precedence;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.parser.util.ListUtil;

public class Call extends AbstractCoreExpr implements CoreExpr {

	private final CoreExpr object;
	private final Key methodName;
	private final List<CoreExpr> arguments;

	Key APPLY_FUNCTION_METHOD_NAME = new Identifier("()");

	public Call(CoreExpr object, Key methodName, List<CoreExpr> arguments) {
		super(object.hashCode() + arguments.hashCode());
		this.object = object;
		this.arguments = arguments;
		this.methodName = methodName;
	}

	public Call(CoreExpr object, Key methodName, CoreExpr ... arguments) {
		this(object, methodName, nonNull(Arrays.asList(arguments)));
	}

	public CoreExpr getObject() {
		return this.object;
	}

	public List<CoreExpr> getArguments() {
		return this.arguments;
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.SUFFIX;
	}

	@Override
	public void toSource(StringBuffer sb) {
		this.object.toSource(sb, Precedence.SUFFIX);
		final boolean applyCall = this.methodName.equals(Method.APPLY_FUNCTION_METHOD_NAME);
		if(!applyCall) {
			sb.append('.');
			this.methodName.toSource(sb);
		}
		if(!this.arguments.isEmpty() || applyCall) {
			sb.append('(');
			boolean first = true;
			for(final CoreExpr arg : this.arguments) {
				if(first) first = false;
				else sb.append(", ");
				arg.toSource(sb, Precedence.COMMA.nextHighest());
			}
			sb.append(')');
		}
	}


	@Override
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.call(this);
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (!(obj instanceof Call))
			return false;
		final Call other = (Call) obj;
		if (!this.arguments.equals(other.arguments))
			return false;
		if (!this.object.equals(other.object))
			return false;
		if (!this.methodName.equals(other.methodName))
			return false;
		return true;
	}

	public Key getMethodName() {
		return this.methodName;
	}

	@Override
	public int compareTo(Expr o) {
		if(this == o)
			return 0;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final Call other = (Call) o;
			if(cmp == 0) cmp = this.object.compareTo(other.object);
			if(cmp == 0) cmp = this.methodName.compareTo(other.methodName);
			if(cmp == 0) cmp = ListUtil.<Expr>compare(this.arguments, other.arguments);
		}
		return cmp;
	}

}
