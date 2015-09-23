package banjo.expr.core;

import banjo.expr.source.Operator;
import banjo.expr.source.Precedence;
import banjo.expr.token.Identifier;
import banjo.expr.token.NumberLiteral;
import banjo.expr.token.StringLiteral;
import banjo.expr.util.ListUtil;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.TreeMap;


public class ObjectLiteral extends AbstractCoreExpr implements CoreExpr {
	public static final ObjectLiteral EMPTY = new ObjectLiteral();
	public static final List<FunctionLiteral> EMPTY_METHOD_LIST = List.nil();
	public final List<Slot> slots;

	public static final Ord<ObjectLiteral> ORD = Slot.LIST_ORD.contramap((ObjectLiteral x) -> x.slots);

	public ObjectLiteral() {
		this(List.nil());
    }

	public ObjectLiteral(Set<SourceFileRange> ranges, List<Slot> slots) {
		super(slots.hashCode()+ranges.hashCode(), ranges);
		this.slots = slots;
	}

	@SafeVarargs
	public ObjectLiteral(Set<SourceFileRange> ranges, Slot ... slots) {
		this(ranges, List.list(slots));
	}

	public ObjectLiteral(Slot slot) {
		this(List.single(slot));
	}

	public ObjectLiteral(List<Slot> slots) {
		this(SourceFileRange.EMPTY_SET, slots);
	}

	@Override
	public Precedence getPrecedence() {
		return Operator.OBJECT_LITERAL.getPrecedence();
	}


	@Override
	public void toSource(final StringBuffer sb) {
		sb.append('{');
		boolean first = true;
		for(final Slot slot : this.slots) {
			if(first) first = false;
			else sb.append(", ");
			slot.toSource(sb);
		}
		sb.append('}');
	}

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.objectLiteral(this);
	}

	@Override
	public <T> T acceptVisitor(final CoreExprAlgebra<T> visitor) {
		return visitor.objectLiteral(getSourceFileRanges(), slots.map(s -> P.p(s.name, s.sourceObjectBinding, s.value.acceptVisitor(visitor))));
	}

	public Option<Slot> findMethod(String name) {
		return findMethods(name).toOption();
	}

	private List<Slot> findMethods(String name) {
	    return slots.filter(s -> name.equals(s.name.id)).reverse();
    }

	public Option<Slot> findMethod(Identifier name) {
		return findMethods(name).toOption();
	}

	private List<Slot> findMethods(Identifier name) {
	    return slots.filter(s -> name.eql(s.name)).reverse();
    }

	public ObjectLiteral withSlots(List<Slot> newSlots) {
		if(newSlots.equals(slots))
			return this;
	    return new ObjectLiteral(getSourceFileRanges(), newSlots);
    }

	public List<Slot> getSlots() {
	    return slots;
    }

	public TreeMap<Identifier,Slot> slotMap() {
		return TreeMap.treeMap(Identifier.ORD, slots.map(s -> P.p(s.name, s)));
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

	@Override
	public String toString() {
		return "{"+ListUtil.insertCommas(slots.map(slot -> slot.name.id + " = ..."))+"}";
	}
}
