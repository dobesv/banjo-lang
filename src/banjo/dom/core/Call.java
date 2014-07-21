package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import fj.F;
import fj.Ord;
import fj.Unit;
import fj.data.List;
import banjo.dom.Expr;
import banjo.dom.ExprAlgebra;
import banjo.dom.source.Operator;
import banjo.dom.source.OperatorType;
import banjo.dom.source.Precedence;
import banjo.dom.token.Key;
import banjo.parser.util.ListUtil;
import banjo.parser.util.SourceFileRange;

public class Call extends AbstractCoreExpr implements CoreExpr {

	private final CoreExpr object;
	private final List<Key> nameParts;
	private final List<List<CoreExpr>> argumentLists;

	public Call(List<SourceFileRange> ranges, CoreExpr object, List<Key> nameParts, List<List<CoreExpr>> argumentLists) {
		super(object.hashCode() + nameParts.hashCode() + argumentLists.hashCode(), ranges);
		this.object = object;
		this.nameParts = nameParts;
		this.argumentLists = argumentLists;
	}

	public Call(List<SourceFileRange> ranges, CoreExpr object, Key methodName, CoreExpr ... arguments) {
		this(ranges, object, List.single(methodName), List.single(List.list(arguments)));
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
		if(nameParts.isEmpty() && argumentLists.length() == 1) {
			return Operator.CALL;
		}
		if(nameParts.length() == 1 && argumentLists.length() == 1) {
			Key methodName = nameParts.head();
			List<CoreExpr> arguments = argumentLists.head();
			final Operator operator = 
					methodName.equals(Method.LOOKUP_METHOD_NAME) ? Operator.LOOKUP
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
				(operator.getOperatorType() == OperatorType.METHOD || (operator.getOperatorType() == OperatorType.LAZY_RHS && isLazyValue(this.argumentLists.head().head())))))) {
			// TODO This isn't handling the lazy operators like ||, &&, ;, => properly
			this.object.toSource(sb, operator.getLeftPrecedence());
			if(operator.isParen()) {
				sb.append(operator.getParenType().getStartChar());
				boolean first = true;
				for(final CoreExpr arg : this.argumentLists.head()) {
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
					this.argumentLists.head().head().acceptVisitor(new BaseCoreExprVisitor<CoreExpr>() {
						@Override
						public CoreExpr fallback() {
							throw new Error("Should be an object literal! (Internal error)");
						}

						@Override
						public CoreExpr objectLiteral(ObjectLiteral n) {
							return n.getMethods().head().getBody();
						}
					}).toSource(sb, operator.getPrecedence());
				} else {
					this.argumentLists.head().head().toSource(sb, operator.getPrecedence());
				}
			}
		} else {

			this.object.toSource(sb, Precedence.SUFFIX);
			sb.append('.');
			List<Key> np = nameParts;
			List<List<CoreExpr>> al = argumentLists;
			while(np.isNotEmpty()) {
				np.head().toSource(sb);
				np = np.tail();
				if(al.isNotEmpty()) {
					sb.append('(');
					boolean first = true;
					for(final CoreExpr arg : al.head()) {
						if(first) first = false;
						else sb.append(", ");
						arg.toSource(sb, Precedence.COMMA.nextHighest());
					}
					sb.append(')');
					al = al.tail();
				} else if(np.isNotEmpty()) {
					sb.append("()");
				}
			}
		}
	}


	private static boolean isLazyValue(@Nullable CoreExpr head) {
		return head != null && head.acceptVisitor(new BaseCoreExprVisitor<Boolean>() {
			@Override
			@SuppressWarnings("null")
			public Boolean fallback() {
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
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
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
		if (!nameParts.equals(other.nameParts))
			return false;
		if (!argumentLists.equals(other.argumentLists))
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
			if(cmp == 0) cmp = this.nameParts.compare(Key.ORD, other.nameParts).toPlusOrMinusOne();
			if(cmp == 0) cmp = this.argumentLists.compare(Ord.listOrd(CoreExpr.ORD), other.argumentLists).toPlusOrMinusOne();
		}
		return cmp;
	}

	@Override
	public <T> T acceptVisitor(final CoreExprAlgebra<T> visitor) {
		return visitor.call(
				getSourceFileRanges(), 
				object.acceptVisitor(visitor),
				nameParts.map(new F<Key, T>() {
					@Override
					public T f(@Nullable Key key) {
						if(key == null) throw new NullPointerException();
						return key.acceptVisitor(visitor);
					}
				}), 
				argumentLists.map(new F<List<CoreExpr>, List<T>>() {
					@Override
					public List<T> f(@Nullable List<CoreExpr> argumentList) {
						if(argumentList == null) throw new NullPointerException();
						return argumentList.map(new F<CoreExpr, T>() {
							@Override
							public T f(@Nullable CoreExpr arg) {
								if(arg == null) throw new NullPointerException();
								return arg.acceptVisitor(visitor);
							}
						});
					}
				})
		);
	}

}
