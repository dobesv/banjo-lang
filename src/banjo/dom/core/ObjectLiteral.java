package banjo.dom.core;

import banjo.dom.Expr;
import banjo.dom.source.Operator;
import banjo.dom.source.Precedence;
import banjo.dom.token.Identifier;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.StringLiteral;
import banjo.parser.util.ExprOrd;
import banjo.parser.util.SourceFileRange;
import fj.F;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.Option;


public class ObjectLiteral extends AbstractCoreExpr implements CoreExpr {
	public static final ObjectLiteral EMPTY = new ObjectLiteral();
	public static final List<FunctionLiteral> EMPTY_METHOD_LIST = List.nil();
	public final List<P2<Identifier,CoreExpr>> slots;

	public static final Ord<FunctionLiteral> ORD = ExprOrd.<FunctionLiteral>exprOrd();

	public ObjectLiteral() {
		this(List.nil());
    }

	public ObjectLiteral(List<SourceFileRange> ranges, List<P2<Identifier,CoreExpr>> slots) {
		super(slots.hashCode()+ranges.hashCode(), ranges);
		this.slots = slots;
	}

	@SafeVarargs
	public ObjectLiteral(List<SourceFileRange> ranges, P2<Identifier,CoreExpr> ... slots) {
		this(ranges, List.list(slots));
	}

	public ObjectLiteral(P2<Identifier,CoreExpr> slot) {
		this(List.single(slot));
	}

	public ObjectLiteral(List<P2<Identifier,CoreExpr>> slots) {
		this(SourceFileRange.EMPTY_LIST, slots);
	}

	@Override
	public Precedence getPrecedence() {
		return Operator.OBJECT_LITERAL.getPrecedence();
	}

	boolean unaryOperatorSlotToSource(StringBuffer sb, Identifier name, CoreExpr value) {
		Operator op = Operator.fromMethodName(name, false);
		if(op == null)
			return false;
		Identifier selfBinding;
		CoreExpr body;
		if(value instanceof Let) {
			Let let = (Let) value;
			if(!(let.bindings.isSingle() && let.bindings.head()._2().compareTo(Identifier.__SELF) == 0))
				return false;
			selfBinding = let.bindings.head()._1();
			body = let.body;
		} else {
			selfBinding = Identifier.__SELF;
			body = value;
		}
		sb.append('(');
		if(op.isPrefix()) {
			op.toSource(sb);
			selfBinding.toSource(sb);
		} else {
			selfBinding.toSource(sb);
			op.toSource(sb);
		}
		sb.append(") = ");
		body.toSource(sb, Operator.ASSIGNMENT.getRightPrecedence());
		return true;
	}

	boolean binaryOperatorSlotToSource(StringBuffer sb, Identifier name, CoreExpr value) {
		Operator op = Operator.fromMethodName(name, true);
		if(op == null)
			return false;
		Identifier selfBinding;
		CoreExpr body;
		if(value instanceof Let) {
			Let let = (Let) value;
			if(!(let.bindings.isSingle() && let.bindings.head()._2().compareTo(Identifier.__SELF) == 0))
				return false;
			selfBinding = let.bindings.head()._1();
			body = let.body;
		} else {
			selfBinding = Identifier.__SELF;
			body = value;
		}
		if(!(body instanceof FunctionLiteral))
			return false;
		FunctionLiteral f = (FunctionLiteral) body;
		if(!f.args.isSingle())
			return false;
		sb.append('(');
		if(op.isSelfOnRightMethodOperator()) {
			f.args.head().toSource(sb);
			sb.append(' ');
			op.toSource(sb);
			sb.append(' ');
			selfBinding.toSource(sb);
		} else {
			selfBinding.toSource(sb);
			sb.append(' ');
			op.toSource(sb);
			sb.append(' ');
			f.args.head().toSource(sb);
		}
		sb.append(") = ");
		f.body.toSource(sb, Operator.ASSIGNMENT.getRightPrecedence());
		return true;
	}
	@Override
	public void toSource(final StringBuffer sb) {
		sb.append('{');
		boolean first = true;
		for(final P2<Identifier, CoreExpr> slot : this.slots) {
			Identifier name = slot._1();
			CoreExpr value = slot._2();
			if(first) first = false;
			else sb.append(", ");
			if(unaryOperatorSlotToSource(sb, name, value) ||
					binaryOperatorSlotToSource(sb, name, value) ||
					methodSlotToSource(sb, name, value, Option.none()))
				continue;

			if(isSelfBinding(value)) {
				((Let)value).bindings.head()._1().toSource(sb);
				sb.append(".");
				value = ((Let)value).body;
			}
			name.toSource(sb);
			if(name.compareTo(value) != 0) {
				sb.append(" = ");
				value.toSource(sb, Operator.ASSIGNMENT.getRightPrecedence());
			}
		}
		sb.append('}');
	}

	public boolean methodSlotToSource(StringBuffer sb, Identifier name, CoreExpr value, Option<Identifier> selfBinding) {
		if(isSelfBinding(value)) {
			return methodSlotToSource(sb, name, ((Let)value).body, Option.some(((Let)value).bindings.head()._1()));
		}

		if(value instanceof FunctionLiteral) {
			FunctionLiteral f = (FunctionLiteral) value;
			selfBinding.forEach(x -> { x.toSource(sb); sb.append('.'); });
			name.toSource(sb);
			sb.append('(');
			int start = sb.length();
			f.args.forEach(a -> { if(sb.length() > start) sb.append(", "); a.toSource(sb); });
			sb.append(") = ");
			f.body.toSource(sb, Operator.ASSIGNMENT.getRightPrecedence());
			return true;
		}
	    return false;
    }

	protected static boolean isSelfBinding(CoreExpr value) {
	    return value instanceof Let && ((Let)value).bindings.isSingle() && ((Let)value).bindings.head()._2().compareTo(Identifier.__SELF) == 0;
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
		if (!this.slots.equals(other.slots))
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
			cmp = Ord.listOrd(Ord.p2Ord(Identifier.ORD, CoreExpr.ORD)).compare(this.slots, other.slots).toInt();
			if(cmp == 0) cmp = super.compareTo(other);
		}
		return cmp;
	}

	@Override
	public <T> T acceptVisitor(final CoreExprAlgebra<T> visitor) {
		return visitor.objectLiteral(getSourceFileRanges(), slots.map(p -> P.p(p._1(), p._2().acceptVisitor(visitor))));
	}

	public Option<CoreExpr> findMethod(String name) {
		return findMethods(name).toOption();
	}

	private List<CoreExpr> findMethods(String name) {
	    return slots.filter(s -> name.equals(s._1().id)).map(P2.__2()).reverse();
    }

	public Option<CoreExpr> findMethod(Identifier name) {
		return findMethods(name).toOption();
	}

	private List<CoreExpr> findMethods(Identifier name) {
	    return slots.filter(s -> name.compareTo(s._1()) == 0).map(P2.__2()).reverse();
    }

	public ObjectLiteral withSlots(List<P2<Identifier,CoreExpr>> newSlots) {
		if(newSlots.equals(slots))
			return this;
	    return new ObjectLiteral(getSourceFileRanges(), newSlots);
    }

	public List<P2<Identifier,CoreExpr>> getSlots() {
	    return slots;
    }

	public static P2<Identifier, CoreExpr> slot(String name, CoreExpr value) {
	    return P.p(new Identifier(name), value);
    }

	public static P2<Identifier, CoreExpr> slot(String name, String value) {
	    return slot(name, new StringLiteral(value));
    }

	public static P2<Identifier, CoreExpr> slot(String name, int value) {
	    return slot(name, new NumberLiteral(value));
    }
}
