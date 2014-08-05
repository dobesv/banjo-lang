package banjo.dom.core;

import static banjo.parser.util.Check.nonNull;

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
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.dom.token.OperatorRef;
import banjo.parser.util.ListUtil;
import banjo.parser.util.SourceFileRange;

public class Call extends AbstractCoreExpr implements CoreExpr {

	private final CoreExpr object;
	private final Key name;
	private final List<List<CoreExpr>> argumentLists;

	public Call(List<SourceFileRange> ranges, CoreExpr object, Key name, List<List<CoreExpr>> argumentLists) {
		super(object.hashCode() + name.hashCode() + argumentLists.hashCode(), ranges);
		this.object = object;
		this.name = name;
		this.argumentLists = argumentLists;
	}

	public Call(List<SourceFileRange> ranges, CoreExpr object, Key methodName, CoreExpr ... arguments) {
		this(ranges, object, methodName, List.single(List.list(arguments)));
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
		List<String> nameParts = name.getParts();
		if(nameParts.isEmpty())
			return Operator.CALL;
		if(!nameParts.isSingle())
			return null;

		boolean binary = argumentLists.isSingle() && argumentLists.head().isSingle();
		boolean unary = argumentLists.isEmpty() || (argumentLists.head().isEmpty() && argumentLists.isSingle());
		if(!(binary || unary))
			return null;

		String methodName = nameParts.head();
		if(binary && methodName.equals(Operator.LOOKUP.getMethodName()))
			return Operator.LOOKUP;

		return Operator.fromMethodName(methodName, binary);
	}

	public List<List<CoreExpr>> getArgumentLists() {
		return argumentLists;
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
		} else if(operator != null && operator.isInfix() && (operator.isParen() || operator.getOperatorType() == OperatorType.METHOD)) {
			// TODO This isn't handling the lazy operators like ||, &&, ;, => properly
			this.object.toSource(sb, operator.getLeftPrecedence());
			if(operator.isParen()) {
				sb.append(operator.getParenType().getStartChar());
				boolean first = true;
				for(final CoreExpr arg : this.getArgumentLists().head()) {
					if(first) first = false;
					else sb.append(", ");
					arg.toSource(sb, Precedence.COMMA.nextHighest());
				}
				sb.append(operator.getParenType().getEndChar());
			} else {
				sb.append(' ');
				operator.toSource(sb);
				sb.append(' ');
				this.getArgumentLists().head().head().toSource(sb, operator.getPrecedence());
			}
		} else {
			this.object.toSource(sb, Precedence.SUFFIX);
			sb.append('.');
			List<String> np = name.getParts();
			List<List<CoreExpr>> al = getArgumentLists();
			while(np.isNotEmpty()) {
				Identifier.toSource(nonNull(np.head()), sb);
				np = np.tail();
				if(al.isNotEmpty()) {
					if(!(al.head().isEmpty() && np.isEmpty())) {
						sb.append('(');
						boolean first = true;
						for(final CoreExpr arg : al.head()) {
							if(first) first = false;
							else sb.append(", ");
							arg.toSource(sb, Precedence.COMMA.nextHighest());
						}
						sb.append(')');
					}
					al = al.tail();
				} else if(np.isNotEmpty()) {
					sb.append("()");
				}
			}
			if(al.isNotEmpty())
				throw new IllegalStateException("Too many argument lists; there should be at most "+name.getParts().length());
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
		if (!name.equals(other.name))
			return false;
		if (!getArgumentLists().equals(other.getArgumentLists()))
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
			if(cmp == 0) cmp = this.name.compareTo(other.name);
			if(cmp == 0) cmp = this.getArgumentLists().compare(Ord.listOrd(CoreExpr.ORD), other.getArgumentLists()).toInt();
		}
		return cmp;
	}

	@Override
	public <T> T acceptVisitor(final CoreExprAlgebra<T> visitor) {
		return visitor.call(
				getSourceFileRanges(),
				object.acceptVisitor(visitor),
				name.acceptVisitor(visitor),
				getArgumentLists().map(new F<List<CoreExpr>, List<T>>() {
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
