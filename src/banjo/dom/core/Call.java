package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Expr;
import banjo.dom.source.Operator;
import banjo.dom.source.OperatorType;
import banjo.dom.source.Precedence;
import banjo.dom.token.Key;
import banjo.parser.util.ListUtil;

public class Call extends AbstractCoreExpr implements CoreExpr {

	private final CoreExpr object;
	private final Key methodName;
	private final fj.data.List<CoreExpr> arguments;

	public static final Key APPLY_FUNCTION_METHOD_NAME = Method.APPLY_FUNCTION_METHOD_NAME;

	public Call(CoreExpr object, Key methodName, fj.data.List<CoreExpr> arguments) {
		super(object.hashCode() + arguments.hashCode());
		this.object = object;
		this.arguments = arguments;
		this.methodName = methodName;
	}

	public Call(CoreExpr object, Key methodName, CoreExpr ... arguments) {
		this(object, methodName, fj.data.List.list(arguments));
	}

	public CoreExpr getObject() {
		return this.object;
	}

	public fj.data.List<CoreExpr> getArguments() {
		return this.arguments;
	}

	@Override
	public Precedence getPrecedence() {
		final Operator operator = Operator.fromMethodName(this.methodName.getKeyString());
		if(operator != null && operator.isPrefix() && this.arguments.isEmpty()) {
			return operator.getPrecedence();
		} else if(operator != null && operator.isSuffix() && this.arguments.isEmpty()) {
			return operator.getPrecedence();
		} else if(operator != null && operator.isInfix() && (operator.isParen() || (this.arguments.isNotEmpty() && this.arguments.tail().isEmpty()))) {
			return operator.getPrecedence();
		}
		return Precedence.SUFFIX;
	}

	@Override
	public void toSource(final StringBuffer sb) {

		final Operator operator = Operator.fromMethodName(this.methodName.getKeyString());
		if(operator != null && operator.isPrefix() && this.arguments.isEmpty()) {
			if(operator.isParen()) {
				// Like |x|
				sb.append(operator.getParenType().getStartChar());
				this.object.toSource(sb, operator.getPrecedence());
				sb.append(operator.getParenType().getEndChar());
			} else {
				operator.toSource(sb);
				this.object.toSource(sb, operator.getPrecedence());
			}
		} else if(operator != null && operator.isSuffix() && this.arguments.isEmpty()) {
			this.object.toSource(sb, operator.getLeftPrecedence());
			operator.toSource(sb);
		} else if(operator != null && operator.isInfix() && (operator.isParen() || (
				this.arguments.isNotEmpty() && this.arguments.tail().isEmpty() &&
				(operator.getOperatorType() == OperatorType.METHOD || (operator.getOperatorType() == OperatorType.LAZY_RHS && isLazyValue(this.arguments.head())))))) {
			// TODO This isn't handling the lazy operators like ||, &&, ;, => properly
			this.object.toSource(sb, operator.getLeftPrecedence());
			if(operator.isParen()) {
				sb.append(operator.getParenType().getStartChar());
				boolean first = true;
				for(final CoreExpr arg : this.arguments) {
					if(first) first = false;
					else sb.append(", ");
					arg.toSource(sb, Precedence.COMMA.nextHighest());
				}
				sb.append(operator.getParenType().getEndChar());
			} else {
				sb.append(' ');
				operator.toSource(sb);
				sb.append(' ');
				if(operator.getOperatorType() == OperatorType.LAZY_RHS) {
					this.arguments.head().acceptVisitor(new BaseCoreExprVisitor<Void>() {
						@Override
						@Nullable
						public Void fallback(CoreExpr unsupported) {
							throw new Error("Should be an object literal! (Internal error)");
						}

						@Override
						@Nullable
						public Void objectLiteral(ObjectLiteral n) {
							n.getMethods().head().getBody().toSource(sb, operator.getPrecedence());
							return null;
						}
					});
				} else {
					this.arguments.head().toSource(sb, operator.getPrecedence());
				}
			}
		} else {

			this.object.toSource(sb, Precedence.SUFFIX);
			sb.append('.');
			this.methodName.toSource(sb);
			if(!this.arguments.isEmpty()) {
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
	}


	private static boolean isLazyValue(@Nullable CoreExpr head) {
		return head != null && head.acceptVisitor(new BaseCoreExprVisitor<Boolean>() {
			@Override
			@SuppressWarnings("null")
			public Boolean fallback(CoreExpr unsupported) {
				return Boolean.FALSE;
			}

			@SuppressWarnings("null")
			@Override
			public Boolean objectLiteral(ObjectLiteral n) {
				return n.isLazyValue();
			}
		}) == Boolean.TRUE;
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
