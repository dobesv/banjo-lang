package banjo.dom.core;

import java.util.Objects;

import banjo.dom.Expr;
import banjo.dom.source.Operator;
import banjo.dom.source.Precedence;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.StringLiteral;
import banjo.parser.util.ExprOrd;
import banjo.parser.util.ListUtil;
import banjo.parser.util.SourceFileRange;
import fj.F;
import fj.Ord;
import fj.data.List;

public class Method extends AbstractCoreExpr implements CoreExpr {

	private final Key selfArg;
	private final Key name;
	private final List<List<Key>> argumentLists;
	private final CoreExpr precondition;
	private final CoreExpr body;
	private final CoreExpr postcondition;

	public static final Ord<Method> ORD = ExprOrd.<Method>exprOrd();

	public static final CoreExpr EMPTY_PRECONDITION = Identifier.TRUE;
	public static final CoreExpr EMPTY_POSTCONDITION = Identifier.TRUE;

	/**
	 *
	 * @param ranges Original source code location
	 * @param selfArg Name given to the receiver of the call, if any
	 * @param name Name of the method
	 * @param argumentLists Names of the methods in the argument lists
	 * @param precondition Expression that must be true for the method to be the right method, it may refer to the arguments
	 * @param body Method body expression
	 * @param postcondition Expression that checks postconditions on the result of the body expression
	 */
	public Method(List<SourceFileRange> ranges, Key selfArg, Key name, List<List<Key>> argumentLists, CoreExpr precondition, CoreExpr body, CoreExpr postcondition) {
		super(calcHash(ranges, selfArg, name, argumentLists, precondition, body, postcondition), ranges);
		this.selfArg = selfArg;
		this.name = name;
		this.argumentLists = argumentLists;
		this.precondition = precondition;
		this.body = body;
		this.postcondition = postcondition;
	}

	public static Method property(Key name, CoreExpr body) {
		return new Method(SourceFileRange.EMPTY_LIST, Key.ANONYMOUS, name, List.nil(), EMPTY_PRECONDITION, body, EMPTY_POSTCONDITION);
	}
	public static Method property(String name, CoreExpr body) {
		return property(new Identifier(name), body);
	}
	private static int calcHash(List<SourceFileRange> ranges, Key selfArg, Key name,
			List<List<Key>> argumentLists, CoreExpr precondition, CoreExpr body, CoreExpr postcondition) {
		final int prime = 31;
		int result = 1;
		result = prime * result + selfArg.hashCode();
		result = prime * result + name.hashCode();
		result = prime * result + argumentLists.hashCode();
		result = prime * result + precondition.hashCode();
		result = prime * result + body.hashCode();
		result = prime * result + postcondition.hashCode();
		result = prime * result + ranges.hashCode();
		return result;
	}


	@Override
	public String toString() {
		final StringBuffer sb = new StringBuffer();
		toSource(sb, "");
		return sb.toString();
	}

	/**
	 * If this method is an operator definition, return which operator it defines.  Otherwise, returns null.
	 */

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
		return Operator.fromMethodName(methodName, binary);

	}

	@Override
	public void toSource(final StringBuffer sb, String idPrefix) {
		// Check for brackets, apply

		final boolean hasSelfName = this.hasSelfArg();
		final Operator operator = getOperator();
		if(operator != null && operator.isInfix() && argumentLists.isSingle() && argumentLists.head().isSingle()) {
			sb.append('(');
			Key arg = argumentLists.head().head();
			if(operator.isParen()) {
				selfArg.toSource(sb, idPrefix);
				sb.append(operator.getParenType().getStartChar());
				arg.toSource(sb, idPrefix);
				sb.append(operator.getParenType().getEndChar());
			} else if(operator.isSelfOnRightMethodOperator()) {
				arg.toSource(sb, idPrefix);
				sb.append(' ');
				operator.toSource(sb);
				sb.append(' ');
				this.selfArg.toSource(sb, idPrefix);
			} else {
				this.selfArg.toSource(sb, idPrefix);
				sb.append(' ');
				operator.toSource(sb);
				sb.append(' ');
				arg.toSource(sb, idPrefix);
			}
			sb.append(')');
		} else if(operator != null && operator.isPrefix()) {
			sb.append('(');
			operator.toSource(sb);
			this.selfArg.toSource(sb, idPrefix);
			sb.append(')');
		} else if(operator != null && operator.isSuffix()) {
			sb.append('(');
			this.selfArg.toSource(sb, idPrefix);
			operator.toSource(sb);
			sb.append(')');
		} else {

			if(hasSelfName) {
				this.selfArg.toSource(sb, idPrefix);
				sb.append('.');
			} else if(this.name.equals(this.body) && this.argumentLists.isEmpty()) {
				this.body.toSource(sb, Precedence.ATOM, idPrefix);
				return;
			}

			List<String> np = name.getParts();
			List<List<Key>> al = argumentLists;
			do {
				if(np.isNotEmpty()) {
					Identifier.toSource(np.head(), sb);
					np = np.tail();
				}
				if(al.isNotEmpty()) {
					sb.append('(');
					for(Key arg : al.head()) {
						if(arg == null) throw new NullPointerException();
						arg.toSource(sb, idPrefix);
					}
					sb.append(')');
					al = al.tail();
				} else if(np.isNotEmpty()) {
					sb.append("()");
				}
			} while(np.isNotEmpty());
		}
		sb.append(' ');
		Operator.ASSIGNMENT.toSource(sb);
		sb.append(' ');
		CoreExpr pre = this.precondition;
		if(!pre.equals(EMPTY_PRECONDITION)) {
			pre.toSource(sb, Operator.PRECONDITION.getLeftPrecedence(), idPrefix);
			Operator.PRECONDITION.toSource(sb);
			this.body.toSource(sb, Operator.PRECONDITION.getRightPrecedence(), idPrefix);
		} else {
			this.body.toSource(sb, Operator.ASSIGNMENT.getRightPrecedence(), idPrefix);
		}
		// TODO postcondition ...
		if(!postcondition.equals(EMPTY_POSTCONDITION)) throw new IllegalStateException("Not implemented... postcondition");
	}

	public CoreExpr getBody() {
		return this.body;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if(obj == null)
			return false;
		if (!super.equals(obj))
			return false;
		if (!(obj instanceof Method))
			return false;
		final Method other = (Method) obj;
		if (!this.name.equals(other.name))
			return false;
		if (Objects.equals(this.precondition, other.precondition))
			return false;
		if (!this.body.equals(other.body))
			return false;
		if (Objects.equals(this.postcondition, other.postcondition))
			return false;
		if (!this.selfArg.equals(other.selfArg))
			return false;
		if (!this.argumentLists.equals(other.argumentLists))
			return false;
		return true;
	}

	@Override
	public int compareTo(Expr o) {
		if(o == null) return -1;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			Method other = (Method) o;
			cmp = name.compareTo(other.name);
			if(cmp == 0) cmp = Ord.listOrd(Ord.listOrd(Key.ORD)).compare(argumentLists, other.argumentLists).toInt();
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
		throw new UnsupportedOperationException();
	}

	@Override
	public <T> T acceptVisitor(final CoreExprAlgebra<T> visitor) {
		return visitor.method(getSourceFileRanges(),
				selfArg.acceptVisitor(visitor),
				name.acceptVisitor(visitor),
				argumentLists.map(new F<List<Key>, List<T>>() {
					@Override
					public List<T> f(List<Key> a) {
						if(a == null) throw new NullPointerException();
						return a.map(new F<Key, T>() {
							@Override
							public T f(Key a) {
								if(a == null) throw new NullPointerException();
								return a.acceptVisitor(visitor);
							}
						});
					}
				}),
				precondition.acceptVisitor(visitor),
				body.acceptVisitor(visitor),
				postcondition.acceptVisitor(visitor));
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

	public CoreExpr getPrecondition() {
		return precondition;
	}

	public CoreExpr getPostcondition() {
		return postcondition;
	}

	public static Method function(Key arg, CoreExpr body) {
		return new Method(SourceFileRange.EMPTY_LIST, Key.ANONYMOUS, Key.ANONYMOUS, List.single(List.single(arg)), EMPTY_PRECONDITION, body, EMPTY_POSTCONDITION);
	}

	public static Method unary(Key name, Key arg, CoreExpr body) {
		return new Method(SourceFileRange.EMPTY_LIST, Key.ANONYMOUS, name, List.single(List.single(arg)), EMPTY_PRECONDITION, body, EMPTY_POSTCONDITION);
	}
	public static Method unary(Operator op, Key arg, CoreExpr body) {
		return new Method(SourceFileRange.EMPTY_LIST, Key.ANONYMOUS, op.getMethodNameKey(), List.single(List.single(arg)), EMPTY_PRECONDITION, body, EMPTY_POSTCONDITION);
	}

	public boolean hasPostcondition() {
		return !postcondition.equals(EMPTY_POSTCONDITION);
	}

	public boolean hasPrecondition() {
		return !precondition.equals(EMPTY_PRECONDITION);
	}

	public static Method call(List<Key> args, CoreExpr body) {
		return simple(Key.ANONYMOUS, args, body);
	}

	private static Method simple(
			Key methodName, List<Key> args,
			CoreExpr body) {
		return new Method(SourceFileRange.EMPTY_LIST, Key.ANONYMOUS, methodName, List.single(args), EMPTY_PRECONDITION, body, EMPTY_POSTCONDITION);
	}

	public static Method property(String name, String value) {
	    return property(name, new StringLiteral(value));
    }
	public static Method property(String name, Number value) {
	    return property(name, new NumberLiteral(value));
    }

	/**
	 * A method with no argument list or a multipart name with no trailing argument list is
	 * called a property and if it is called with an (extra) argument list the extra lists are
	 * actually passed to the result of getting the property value, assuming it is returning
	 * a callable object.
	 */
	public boolean isProperty() {
	    return getArgumentLists().isEmpty() || (getName().getParts().length() > getArgumentLists().length());
    }

}