package banjo.dom.core;

import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.Nullable;

import fj.F;
import fj.Ord;
import fj.Unit;
import fj.data.List;
import banjo.dom.Expr;
import banjo.dom.source.Operator;
import banjo.dom.source.Operator.Position;
import banjo.dom.source.Precedence;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.dom.token.OperatorRef;
import banjo.parser.util.AbstractCachedHashCode;
import banjo.parser.util.ExprOrd;
import banjo.parser.util.ListUtil;
import banjo.parser.util.SourceFileRange;

public class Method extends AbstractCoreExpr implements CoreExpr {

	private final Key selfArg;
	private final Key name;
	private final List<List<Key>> argumentLists;
	private final CoreExpr body;

	public static final Ord<Method> ORD = ExprOrd.<Method>exprOrd();

	public Method(List<SourceFileRange> ranges, Key selfArg, Key name, List<List<Key>> argumentLists, CoreExpr body) {
		super(calcHash(ranges, selfArg, name, argumentLists, body), ranges);
		this.selfArg = selfArg;
		this.name = name;
		this.argumentLists = argumentLists;
		this.body = body;
	}

	public static Method nullary(Key name, CoreExpr body) {
		return new Method(SourceFileRange.EMPTY_LIST, Key.ANONYMOUS, name, List.<List<Key>>nil(), body);
	}
	private static int calcHash(List<SourceFileRange> ranges, Key selfArg, Key name,
			List<List<Key>> argumentLists, CoreExpr body) {
		final int prime = 31;
		int result = 1;
		result = prime * result + selfArg.hashCode();
		result = prime * result + name.hashCode();
		result = prime * result + argumentLists.hashCode();
		result = prime * result + body.hashCode();
		result = prime * result + ranges.hashCode();
		return result;
	}

	@SuppressWarnings("null")
	@Override
	public String toString() {
		final StringBuffer sb = new StringBuffer();
		toSource(sb);
		return sb.toString();
	}

	/**
	 * If this method is an operator definition, return which operator it defines.  Otherwise, returns null.
	 */
	@Nullable
	Operator getOperator() {
		// Operators always define a simply named "self" part
		if(selfArg instanceof MixfixFunctionIdentifier || selfArg instanceof AnonymousKey)
			return null;
		// Operator has zero (unary operator) or one (binary operator) arguments
		boolean unary = argumentLists.isEmpty() || (argumentLists.isSingle() && argumentLists.head().isEmpty());
		boolean binary = argumentLists.isSingle() && argumentLists.head().isSingle();
		if(!(unary || binary))
			return null;
		if(name instanceof AnonymousKey && binary)
			return Operator.CALL;
		if(!(name instanceof Identifier))
			return null;
		String methodName = ((Identifier)name).getId();
		if(binary && methodName.equals(Operator.LOOKUP.getMethodName())) {
			return Operator.LOOKUP;
		}
		return Operator.fromMethodName(methodName, binary);

	}

	public void toSource(final StringBuffer sb) {
		// Check for brackets, apply

		final boolean hasSelfName = this.hasSelfArg();
		final Operator operator = getOperator();
		if(operator != null && operator.isInfix()) {
			sb.append('(');
			Key arg = argumentLists.head().head();
			if(operator.isParen()) {
				selfArg.toSource(sb);
				sb.append(operator.getParenType().getStartChar());
				arg.toSource(sb);
				sb.append(operator.getParenType().getEndChar());
			} else if(operator.isSelfOnRightMethodOperator()) {
				arg.toSource(sb);
				sb.append(' ');
				operator.toSource(sb);
				sb.append(' ');
				this.selfArg.toSource(sb);
			} else {
				this.selfArg.toSource(sb);
				sb.append(' ');
				operator.toSource(sb);
				sb.append(' ');
				arg.toSource(sb);
			}
			sb.append(')');
		} else if(operator != null && operator.isPrefix()) {
			sb.append('(');
			operator.toSource(sb);
			this.selfArg.toSource(sb);
			sb.append(')');
		} else if(operator != null && operator.isSuffix()) {
			sb.append('(');
			this.selfArg.toSource(sb);
			operator.toSource(sb);
			sb.append(')');
		} else {

			if(hasSelfName) {
				this.selfArg.toSource(sb);
				sb.append('.');
			} else if(this.name.equals(this.body) && this.argumentLists.isEmpty()) {
				this.body.toSource(sb);
				return;
			}

			List<String> np = name.getParts();
			List<List<Key>> al = argumentLists;
			while(np.isNotEmpty()) {
				Identifier.toSource(nonNull(np.head()), sb);
				np = np.tail();
				if(al.isNotEmpty()) {
					if(!(al.head().isEmpty() && np.isEmpty())) {
						sb.append('(');
						for(Key arg : al.head()) {
							if(arg == null) throw new NullPointerException();
							arg.toSource(sb);
						}
						sb.append(')');
					}
					al = al.tail();
				} else if(np.isNotEmpty()) {
					sb.append("()");
				}
			}
		}
		sb.append(' ');
		Operator.ASSIGNMENT.toSource(sb);
		sb.append(' ');
		this.body.toSource(sb, Precedence.COLON);
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
		if (!this.name.equals(other.name))
			return false;
		if (!this.body.equals(other.body))
			return false;
		if (!this.selfArg.equals(other.selfArg))
			return false;
		if (!this.argumentLists.equals(other.argumentLists))
			return false;
		return true;
	}

	@Override
	public int compareTo(@Nullable Expr o) {
		if(o == null) return -1;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			Method other = (Method) o;
			cmp = name.compareTo(other.name);
			if(cmp == 0) cmp = argumentLists.compare(Ord.listOrd(Key.ORD), other.argumentLists).toInt();
			if(cmp == 0) cmp = selfArg.compareTo(other.selfArg);
			if(cmp == 0) cmp = this.body.compareTo(other.body);
			if(cmp == 0) cmp = ListUtil.compare(this.getSourceFileRanges(), other.getSourceFileRanges());
		}
		return cmp;
	}

	public boolean hasSelfArg() {
		return !(this.selfArg instanceof AnonymousKey);
	}

	/**
	 * True if this method could have been created using the lambda syntax (x,...) -> y
	 */
	public boolean isSimpleApplyMethod() {
		return (this.name instanceof AnonymousKey);
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ASSIGNMENT;
	}

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.method(this);
	}

	@Override
	public <T> T acceptVisitor(final CoreExprAlgebra<T> visitor) {
		return visitor.method(getSourceFileRanges(),
				selfArg.acceptVisitor(visitor),
				name.acceptVisitor(visitor),
				argumentLists.map(new F<List<Key>, List<T>>() {
					@Override
					public List<T> f(@Nullable List<Key> a) {
						if(a == null) throw new NullPointerException();
						return a.map(new F<Key, T>() {
							@Override
							public T f(@Nullable Key a) {
								if(a == null) throw new NullPointerException();
								return a.acceptVisitor(visitor);
							}
						});
					}
				}),
				body.acceptVisitor(visitor));
	}

	public Key getSelfArg() {
		return selfArg;
	}

	public Key getName() {
		return name;
	}

	public List<List<Key>> getArgumentLists() {
		return argumentLists;
	}

	public int countArgumentLists() {
		return argumentLists.length();
	}

	public int totalDeclaredArguments() {
		int i=0;
		for(List<Key> argList : argumentLists) {
			i += argList.length();
		}
		return i;
	}

}