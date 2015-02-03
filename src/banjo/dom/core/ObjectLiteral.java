package banjo.dom.core;

import banjo.dom.Expr;
import banjo.dom.source.Operator;
import banjo.dom.source.Precedence;
import banjo.dom.token.Identifier;
import banjo.dom.token.StringLiteral;
import banjo.parser.SourceCodeScanner;
import banjo.parser.util.ExprOrd;
import banjo.parser.util.SourceFileRange;
import fj.F;
import fj.Ord;
import fj.P2;
import fj.Unit;
import fj.data.List;


public class ObjectLiteral extends AbstractCoreExpr implements CoreExpr {
	public static final ObjectLiteral EMPTY = new ObjectLiteral();
	public static final fj.data.List<FunctionLiteral> EMPTY_METHOD_LIST = fj.data.List.nil();
	private final fj.data.List<P2<Identifier,CoreExpr>> slots;

	public static final Ord<FunctionLiteral> ORD = ExprOrd.<FunctionLiteral>exprOrd();

	public ObjectLiteral() {
		this(List.nil());
    }

	public ObjectLiteral(List<SourceFileRange> ranges, fj.data.List<P2<Identifier,CoreExpr>> slots) {
		super(slots.hashCode()+ranges.hashCode(), ranges);
		this.slots = slots;
	}

	@SafeVarargs
	public ObjectLiteral(List<SourceFileRange> ranges, P2<Identifier,CoreExpr> ... slots) {
		this(ranges, fj.data.List.list(slots));
	}

	public ObjectLiteral(P2<Identifier,CoreExpr> ... slots) {
		this(fj.data.List.list(slots));
	}

	public ObjectLiteral(fj.data.List<P2<Identifier,CoreExpr>> slots) {
		this(SourceFileRange.EMPTY_LIST, slots);
	}

	@Override
	public Precedence getPrecedence() {
		return isSelector() ? Operator.SELECTOR.getPrecedence() :
			isLambda() ? Operator.FUNCTION.getPrecedence() :
				Precedence.ATOM;
	}

	@Override
	public void toSource(final StringBuffer sb) {
		sb.append('{');
		boolean first = true;
		for(final P2<Identifier, CoreExpr> method : this.slots) {
			if(first) first = false;
			else sb.append(", ");
			method.toSource(sb);
		}
		sb.append('}');
	}

	public boolean isSelector() {
	    return methods.isSingle()
	    		&& methods.head().getArgumentLists().isSingle()
	    		&& methods.head().getArgumentLists().head().isSingle()
	    		&& methods.head().getBody() instanceof Call
	    		&& ((Call)methods.head().getBody()).getObject().equals(methods.head().getArgumentLists().head().head())
	    		&& methods.head().getName().equals(Identifier.ANONYMOUS);
    }

	private void argListToSource(final StringBuffer sb, final FunctionLiteral method, String idPrefix) {
		sb.append('(');
		boolean first = true;
		for(Identifier arg : method.getArgumentLists().head()) {
			if(first) first = false;
			else sb.append(", ");
			arg.toSource(sb);
		}
		sb.append(')');
	}

	public boolean isLambda() {
		return this.methods.isSingle() && this.methods.head().isSimpleApplyMethod();
	}

	public static StringBuffer maybeQuoteIdentifier(String identifier, StringBuffer sb) {
		for(int i=0; i < identifier.length(); i++) {
			final int cp = identifier.codePointAt(i);
			if(cp > Character.MAX_VALUE) i++; // Actually a pair of characters
			final boolean ok = i==0 ? SourceCodeScanner.isIdentifierStart(cp):SourceCodeScanner.isIdentifierPart(cp);
			if(!ok) {
				return StringLiteral.toSource(identifier, sb);
			}
		}
		sb.append(identifier);
		return sb;
	}


	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.objectLiteral(this);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!super.equals(obj))
			return false;
		if (!(obj instanceof ObjectLiteral))
			return false;
		final ObjectLiteral other = (ObjectLiteral) obj;
		if (!this.methods.equals(other.methods))
			return false;
		return true;
	}

	@Override
	public int compareTo(Expr o) {
		if(this == o)
			return 0;
		if(o == null)
			return -1;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final ObjectLiteral other = (ObjectLiteral) o;
			cmp = Ord.listOrd(FunctionLiteral.ORD).compare(this.methods, other.methods).toInt();
			if(cmp == 0) cmp = super.compareTo(other);
		}
		return cmp;
	}

	public boolean isLazyValue() {
		return isLambda() && (this.methods.head().getArgumentLists().head().isEmpty());
	}

	@Override
	public <T> T acceptVisitor(final CoreExprAlgebra<T> visitor) {
		return visitor.objectLiteral(getSourceFileRanges(), methods.map(new F<FunctionLiteral, T>() {
			@Override
			public T f(FunctionLiteral a) {
				if(a == null) throw new NullPointerException();
				return a.acceptVisitor(visitor);
			}
		}));
	}

	public FunctionLiteral findMethod(String name) {
		return findMethods(name).toOption().toNull();
	}

	private List<FunctionLiteral> findMethods(String name) {
	    return methods.filter(m -> (m.name instanceof Identifier) && name.equals(((Identifier)m.name).id)).reverse();
    }

	public FunctionLiteral findMethod(Identifier name) {
		return findMethods(name).toOption().toNull();
	}

	private List<FunctionLiteral> findMethods(Identifier name) {
		final Identifier baseName = name.withoutPrefix();
	    return methods.filter(m -> baseName.compareTo(m.name) == 0).reverse();
    }

	public ObjectLiteral withSlots(List<P2<Identifier,CoreExpr>> newSlots) {
		if(newSlots.equals(slots))
			return this;
	    return new ObjectLiteral(getSourceFileRanges(), newSlots);
    }

	public fj.data.List<P2<Identifier,CoreExpr>> getSlots() {
	    return slots;
    }
}
