package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import fj.data.List;
import banjo.dom.Expr;
import banjo.dom.source.Operator;
import banjo.dom.source.OperatorType;
import banjo.dom.source.Precedence;
import banjo.dom.token.Key;
import banjo.parser.util.ListUtil;
import banjo.parser.util.SourceFileRange;

public class Call extends AbstractCoreExpr implements CoreExpr {

	private final CoreExpr object;
	private final List<MessagePart> parts;

	public static final Key APPLY_FUNCTION_METHOD_NAME = Method.APPLY_FUNCTION_METHOD_NAME;

	public static final class MessagePart implements Comparable<MessagePart> {
		private final Key key;
		private final fj.data.List<CoreExpr> arguments;
		public MessagePart(Key key, List<CoreExpr> arguments) {
			super();
			this.key = key;
			this.arguments = arguments;
		}
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + arguments.hashCode();
			result = prime * result + key.hashCode();
			return result;
		}
		@Override
		public boolean equals(@Nullable Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			MessagePart other = (MessagePart) obj;
			if (!arguments.equals(other.arguments))
				return false;
			if (!key.equals(other.key))
				return false;
			return true;
		}
		public Key getKey() {
			return key;
		}
		public fj.data.List<CoreExpr> getArguments() {
			return arguments;
		}
		
		@Override
		public int compareTo(@Nullable MessagePart o) {
			if(o == null) return -1;
			int cmp = key.compareTo(o.key);
			if(cmp == 0) return ListUtil.<Expr>compare(arguments, o.arguments);
			return 0;
		}
	}
	
	public Call(SourceFileRange sfr, CoreExpr object, fj.data.List<MessagePart> parts) {
		super(object.hashCode() + parts.hashCode(), sfr);
		this.object = object;
		this.parts = parts;
	}

	public Call(SourceFileRange sfr, CoreExpr object, Key methodName, CoreExpr ... arguments) {
		this(sfr, object, fj.data.List.single(new MessagePart(methodName, fj.data.List.list(arguments))));
	}

	public CoreExpr getObject() {
		return this.object;
	}

	@Override
	public Precedence getPrecedence() {
		final Operator operator = getOperator();
		
		if(operator != null) {
			return operator.getPrecedence();
		}
		return Precedence.SUFFIX;
	}

	private @Nullable Operator getOperator() {
		if(parts.length() == 1) {
			Key methodName = parts.head().key;
			List<CoreExpr> arguments = parts.head().arguments;
			final Operator operator = 
					methodName.equals(APPLY_FUNCTION_METHOD_NAME) ? Operator.CALL
					: methodName.equals(Method.LOOKUP_METHOD_NAME) ? Operator.LOOKUP
					: arguments.isEmpty() ? Operator.fromMethodName(methodName.getKeyString(), false)
					: arguments.length() == 1 ? Operator.fromMethodName(methodName.getKeyString(), true)
					: null;
			return operator;
		} else {
			return null;
		}
	}

	@Override
	public void toSource(final StringBuffer sb) {

		final Operator operator = this.getOperator();
		if(operator != null && operator.isPrefix()) {
			if(operator.isParen()) {
				// Like |x|
				sb.append(operator.getParenType().getStartChar());
				this.object.toSource(sb, operator.getPrecedence());
				sb.append(operator.getParenType().getEndChar());
			} else {
				operator.toSource(sb);
				this.object.toSource(sb, operator.getPrecedence());
			}
		} else if(operator != null && operator.isSuffix()) {
			this.object.toSource(sb, operator.getLeftPrecedence());
			operator.toSource(sb);
		} else if(operator != null && operator.isInfix() && (operator.isParen() || (
				(operator.getOperatorType() == OperatorType.METHOD || (operator.getOperatorType() == OperatorType.LAZY_RHS && isLazyValue(this.parts.head().arguments.head())))))) {
			// TODO This isn't handling the lazy operators like ||, &&, ;, => properly
			this.object.toSource(sb, operator.getLeftPrecedence());
			if(operator.isParen()) {
				sb.append(operator.getParenType().getStartChar());
				boolean first = true;
				for(final CoreExpr arg : this.parts.head().arguments) {
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
					this.parts.head().arguments.head().acceptVisitor(new BaseCoreExprVisitor<Void>() {
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
					this.parts.head().arguments.head().toSource(sb, operator.getPrecedence());
				}
			}
		} else {

			this.object.toSource(sb, Precedence.SUFFIX);
			sb.append('.');
			boolean prevPartNoArgs = false;
			boolean firstPart = true;
			for(MessagePart part : parts) {
				if(firstPart) firstPart = false;
				else if(prevPartNoArgs) sb.append("() ");
				else sb.append(" ");
				
				part.key.toSource(sb);
				if(!part.arguments.isEmpty()) {
					sb.append('(');
					boolean first = true;
					for(final CoreExpr arg : part.arguments) {
						if(first) first = false;
						else sb.append(", ");
						arg.toSource(sb, Precedence.COMMA.nextHighest());
					}
					sb.append(')');
					prevPartNoArgs = false;
				} else {
					prevPartNoArgs = true;
				}
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
		if (!this.object.equals(other.object))
			return false;
		if (!this.parts.equals(other.parts))
			return false;
		return true;
	}

	@Override
	public int compareTo(@Nullable Expr o) {
		if(o == null) throw new NullPointerException();
		if(this == o)
			return 0;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final Call other = (Call) o;
			if(cmp == 0) cmp = this.object.compareTo(other.object);
			if(cmp == 0) cmp = ListUtil.compare(this.parts, other.parts);
		}
		return cmp;
	}

	public List<MessagePart> getParts() {
		return parts;
	}

}
