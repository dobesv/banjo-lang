package banjo.expr.core;

import banjo.expr.source.Operator;
import banjo.expr.source.Precedence;
import banjo.expr.util.ListUtil;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.P;
import fj.data.List;
import fj.data.Set;


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
		return visitor.objectLiteral(getRanges(), slots.map(s -> P.p(s.name, s.sourceObjectBinding, s.value.acceptVisitor(visitor))));
	}

	public List<Slot> getSlots() {
	    return slots;
    }

	@Override
	public String toString() {
		return "{"+ListUtil.insertCommas(slots.map(slot -> slot.name.id + " = ..."))+"}";
	}
}
