package banjo.eval.expr;

import banjo.eval.resolver.ClosureResolver;
import banjo.eval.resolver.GlobalRef;
import banjo.eval.resolver.InstanceAlgebra;
import banjo.eval.resolver.NameRef;
import banjo.eval.resolver.NameRefAlgebra;
import banjo.expr.free.FreeExpression;
import banjo.expr.util.OrdUtil;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.P2;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.Stream;
import fj.data.TreeMap;

/**
 * An open slot instance may be affected by extension operations because its
 * calculation depends on the value of slots in its object.
 * <p>
 * Note that the body should already have any global / free variables embedded
 * in it.
 */
public class OpenSlotInstance<T> implements SlotInstance<T> {
    public final String name;
    public final String slotObjectRef;
	public final FreeExpression body;
    public final TreeMap<NameRef, T> closure;

    public OpenSlotInstance(String name, String slotObjectRef, FreeExpression body, TreeMap<NameRef, T> closure) {
		this.name = name;
        this.slotObjectRef = slotObjectRef;
		this.body = body;
        this.closure = closure;
    }

    /**
     * Calculate the value or type of the slot according to the actual object
     * the slot is being read from
     * 
     * @param trace
     *            Trace of how this computation was reached, for debugging
     * @param object
     *            Object the slot is being read from; if the object is an
     *            extension or extended, this might not be the object the slot
     *            is defined in.
     */
	@Override
    public T apply(List<T> trace, T object, T prevSlotValue, InstanceAlgebra<T> algebra) {
        NameRefAlgebra<Option<T>> localResolver = new NameRefAlgebra<Option<T>>() {
            @Override
            public String toString() {
                return "{" + object + ".name" + " = ... }";
            }

            @Override
            public Option<T> local(Set<SourceFileRange> ranges, String name) {
                if(name.equals(slotObjectRef))
                    return Option.some(object);

                return Option.none();
            }

            @Override
            public Option<T> slot(NameRef object, Set<SourceFileRange> ranges, String slotName) {
                return visit(object).map(obj -> algebra.slotValue(obj, ranges, slotName));
            }

            @Override
            public Option<T> baseSlot(Set<SourceFileRange> ranges, String slotObjectRef2, String slotName) {
                if(slotObjectRef.equals(slotObjectRef2) && slotName.equals(name)) {
                    return Option.fromNull(prevSlotValue);
                } 
                return Option.none();
            }

            @Override
            public Option<T> functionBase(Set<SourceFileRange> ranges, String functionSelfName) {
                return Option.none();
            }

            @Override
            public Option<T> invalid(Set<SourceFileRange> ranges, String reason) {
                return Option.none();
            }

            @Override
            public Option<T> global(GlobalRef globalRef) {
                return Option.none();
            }
        };
        return body.eval(trace, new ClosureResolver<T>(closure, algebra, localResolver), algebra);
	}

	@Override
	public String toString() {
        return slotObjectRef + "." + name;
	}

    @Override
    public <V> V acceptVisitor(SlotInstanceVisitor<T, V> visitor) {
        return visitor.open(this);
    }

    public String getName() {
        return name;
    }

    public String getSlotObjectRef() {
        return slotObjectRef;
    }

    public FreeExpression getBody() {
        return body;
    }

    public TreeMap<NameRef, T> getClosure() {
        return closure;
    }

    public Stream<P2<NameRef, T>> getClosurePairs() {
        return closure.toStream();
    }

    public static <T> Ord<OpenSlotInstance<T>> ord(Ord<T> ord) {
        return OrdUtil.chain(
            Ord.stringOrd.contramap(OpenSlotInstance::getName),
            Ord.stringOrd.contramap(OpenSlotInstance::getSlotObjectRef),
            FreeExpression.ORD.contramap(OpenSlotInstance::getBody),
            Ord.streamOrd(Ord.p2Ord(NameRef.ORD, ord)).contramap(OpenSlotInstance::getClosurePairs));
    }

}
