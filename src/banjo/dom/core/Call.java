package banjo.dom.core;

import static banjo.parser.util.Check.nonNull;
import banjo.dom.Expr;
import banjo.dom.source.Operator;
import banjo.dom.source.OperatorType;
import banjo.dom.source.Precedence;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.parser.util.SourceFileRange;
import fj.F;
import fj.Ord;
import fj.data.List;

public class Call extends AbstractCoreExpr implements CoreExpr {

	private final CoreExpr object;
	private final Key name;
	private final List<List<CoreExpr>> argumentLists;
	private final boolean callNext;
	private final boolean optional;

	public Call(List<SourceFileRange> ranges, CoreExpr object, Key name, List<List<CoreExpr>> argumentLists, boolean callNext, boolean optional) {
		super(object.hashCode() + name.hashCode() + argumentLists.hashCode(), ranges);
		this.object = object;
		this.name = name;
		this.argumentLists = argumentLists;
		this.callNext = callNext;
		this.optional = optional;
	}

	public Call(List<SourceFileRange> ranges, CoreExpr object, Key methodName, CoreExpr ... arguments) {
		this(ranges, object, methodName, List.single(List.list(arguments)), false, false);
	}
	public Call(List<SourceFileRange> ranges, CoreExpr object, Key methodName, boolean optional, CoreExpr ... arguments) {
		this(ranges, object, methodName, List.single(List.list(arguments)), false, optional);
	}
	public Call(CoreExpr object, Key methodName, CoreExpr argument) {
		this(SourceFileRange.EMPTY_LIST, object, methodName, List.single(List.single(argument)), false, false);
	}
	public Call(CoreExpr object, Key methodName, List<CoreExpr> arguments) {
		this(SourceFileRange.EMPTY_LIST, object, methodName, List.single(arguments), false, false);
	}

	public static Call getter(CoreExpr a, Key name) {
		return new Call(SourceFileRange.EMPTY_LIST, a, name, List.nil(), false, false);
	}
	public static Call binaryOp(CoreExpr a, Operator op, CoreExpr arg) {
		return new Call(a, op.getMethodNameKey(), List.single(arg));
	}

	public static Call unaryOp(CoreExpr a, Operator op) {
		return new Call(a, op.getMethodNameKey(), List.nil());
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

	private Operator getOperator() {
		if(optional || callNext)
			return null;
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
		return Operator.fromMethodName(methodName, binary);
	}

	public List<List<CoreExpr>> getArgumentLists() {
		return argumentLists;
	}

	/**
	 * Flatten the argument lists into a single list
	 * @return
	 */
	public List<CoreExpr> getAllArguments() {
		return List.join(argumentLists);
	}

	@Override
	public void toSource(final StringBuffer sb, final String idPrefix) {

		final Operator operator = this.getOperator();
		if(operator != null && operator.isPrefix()) {
			if(operator.isParen()) {
				// Like |x|
				sb.append(operator.getParenType().getStartChar());
				this.object.toSource(sb, operator.getPrecedence(), idPrefix);
				sb.append(operator.getParenType().getEndChar());
			} else {
				operator.toSource(sb);
				this.object.toSource(sb, operator.getPrecedence(), idPrefix);
			}
		} else if(operator != null && operator.isSuffix()) {
			this.object.toSource(sb, operator.getLeftPrecedence(), idPrefix);
			operator.toSource(sb);
		} else if(operator == Operator.CALL && this.object instanceof MixfixFunctionIdentifier) {
			argsToSource(sb, ((MixfixFunctionIdentifier)this.object).getParts(), idPrefix);
		} else if(operator != null && operator.isInfix() && (operator.isParen() || operator.getOperatorType() == OperatorType.METHOD)) {
			this.object.toSource(sb, operator.getLeftPrecedence(), idPrefix);
			if(operator.isParen()) {
				sb.append(operator.getParenType().getStartChar());
				boolean first = true;
				for(final CoreExpr arg : this.getArgumentLists().head()) {
					if(first) first = false;
					else sb.append(", ");
					arg.toSource(sb, Precedence.COMMA.nextHighest(), idPrefix);
				}
				sb.append(operator.getParenType().getEndChar());
			} else {
				sb.append(' ');
				operator.toSource(sb);
				sb.append(' ');
				this.getArgumentLists().head().head().toSource(sb, operator.getPrecedence(), idPrefix);
			}
		} else {
			this.object.toSource(sb, Precedence.SUFFIX, idPrefix);
			projectionToSource(sb, idPrefix);
		}
	}

	public void projectionToSource(final StringBuffer sb, String idPrefix) {
	    final List<String> np = name.getParts();
	    final List<String> np2 = np.isEmpty() ? List.single("") : np;
	    Operator op = optional ? callNext ? Operator.OPT_CALL_NEXT_METHOD : Operator.OPT_PROJECTION : callNext ? Operator.CALL_NEXT_METHOD : Operator.PROJECTION;
	    op.toSource(sb);
    	argsToSource(sb, np2, idPrefix);
    }

	private void argsToSource(final StringBuffer sb, List<String> np, String idPrefix) {
	    List<List<CoreExpr>> al = getArgumentLists();
	    while(np.isNotEmpty()) {
	    	Identifier.toSource(nonNull(np.head()), sb);
	    	if(np.isNotEmpty()) np = np.tail();
	    	if(al.isNotEmpty()) {
    			sb.append('(');
    			boolean first = true;
    			for(final CoreExpr arg : al.head()) {
    				if(first) first = false;
    				else sb.append(", ");
    				arg.toSource(sb, Precedence.COMMA.nextHighest(), idPrefix);
    			}
    			sb.append(')');
	    		al = al.tail();
	    	} else if(np.isNotEmpty()) {
	    		sb.append("()");
	    	}
	    }
	    if(al.isNotEmpty())
	    	throw new IllegalStateException("Too many argument lists; there should be at most "+name.getParts().length()+", got "+getArgumentLists().length());
    }

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.call(this);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!(obj instanceof Call))
			return false;
		if (!super.equals(obj))
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
	public int compareTo(Expr o) {
		if(o == null) throw new NullPointerException();
		if(this == o)
			return 0;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final Call other = (Call) o;
			if(cmp == 0) cmp = this.object.compareTo(other.object);
			if(cmp == 0) cmp = this.name.compareTo(other.name);
			if(cmp == 0) cmp = Ord.listOrd(Ord.listOrd(CoreExpr.ORD)).compare(this.getArgumentLists(), other.getArgumentLists()).toInt();
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
					public List<T> f(List<CoreExpr> argumentList) {
						if(argumentList == null) throw new NullPointerException();
						return argumentList.map(new F<CoreExpr, T>() {
							@Override
							public T f(CoreExpr arg) {
								if(arg == null) throw new NullPointerException();
								return arg.acceptVisitor(visitor);
							}
						});
					}
				}),
				optional,
				callNext
		);
	}

	public static Call callFunction(CoreExpr func, List<CoreExpr> args) {
		return new Call(List.nil(), func, Key.ANONYMOUS, List.single(args), false, false);
	}

	public static CoreExpr operator(CoreExpr object, Operator operator, CoreExpr arg) {
		return new Call(List.nil(), object, new Identifier(operator.getMethodName()), List.single(List.single(arg)), false, false);
	}

	public Key getName() {
		return name;
	}

	public boolean isCallNext() {
		return callNext;
	}

	public boolean isOptional() {
		return optional;
	}

}
