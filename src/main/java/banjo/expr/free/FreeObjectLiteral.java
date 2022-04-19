package banjo.expr.free;

import banjo.eval.EvalContext;
import banjo.eval.expr.SlotInstance;
import banjo.eval.resolver.InstanceAlgebra;
import banjo.eval.resolver.NameRef;
import banjo.eval.resolver.NameRefAlgebra;
import banjo.eval.resolver.Resolver;
import banjo.expr.util.ListUtil;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.P3;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.TreeMap;

public class FreeObjectLiteral implements FreeExpression {
	public final Set<SourceFileRange> ranges;
    public final List<P3<String, Option<String>, FreeExpression>> slots;

    private Set<NameRef> freeRefs;

    public FreeObjectLiteral(Set<SourceFileRange> ranges,
        List<P3<String, Option<String>, FreeExpression>> slots) {
		this.ranges = ranges;
		this.slots = slots;
    }

    public FreeObjectLiteral(TreeMap<String, FreeExpression> slots) {
        this(SourceFileRange.EMPTY_SET, slots.toStream().map(p -> P.p(p._1(), Option.<String> none(), p._2())).toList());
    }

    public static Set<NameRef> slotFreeRefs(String slotName, Option<String> slotObjectRef, FreeExpression body) {
        Set<NameRef> bodyRefs = body.getFreeRefs();
        return slotObjectRef.map(r -> bodyRefs.filter(ref -> isFree(r, ref))).orSome(bodyRefs);

    }
    private static Set<NameRef> calculateFreeRefs(List<P3<String, Option<String>, FreeExpression>> slots) {
        return Set.join(NameRef.ORD, Set.iterableSet(Ord.setOrd(NameRef.ORD), slots.map(p -> slotFreeRefs(p._1(), p._2(), p._3()))));
    }

    public static boolean isFree(String slotObjectRef, NameRef ref) {
        return ref.acceptVisitor(new NameRefAlgebra<Boolean>() {

            @Override
            public Boolean local(Set<SourceFileRange> ranges, String name) {
                // Filter out the slot object ref
                return !slotObjectRef.equals(name);
            }

            @Override
            public Boolean slot(NameRef object, Set<SourceFileRange> ranges, String slotName) {
                // If its a slot on the slot object ref, filter it out
                return object.acceptVisitor(this);
            }

            @Override
            public Boolean baseSlot(Set<SourceFileRange> ranges, String slotObjectRefId, String slotName) {
                return local(ranges, slotObjectRefId);
            }

            @Override
            public Boolean functionBase(Set<SourceFileRange> ranges, String functionSelfName) {
                return local(ranges, functionSelfName);
            }

            @Override
            public Boolean invalid(Set<SourceFileRange> ranges, String reason) {
                return true;
            }

        });
    }

    public FreeObjectLiteral(Set<SourceFileRange> ranges, List<P3<String, Option<String>, FreeExpression>> slots, Set<NameRef> freeRefs) {
        super();
        this.ranges = ranges;
        this.slots = slots;
        this.freeRefs = freeRefs;
    }

    @Override
    public Set<NameRef> getFreeRefs() {
        if(this.freeRefs == null) {
            this.freeRefs = calculateFreeRefs(slots);
        }
        return freeRefs;
    }

    @Override
    public boolean hasFreeRefs() {
        return !freeRefs.isEmpty();
    }

    public P3<String, Option<String>, FreeExpression> partialBindSlot(P3<String, Option<String>, FreeExpression> p,
        PartialResolver resolver) {
        String slotName = p._1();
        Option<String> slotObjectRef = p._2();
        FreeExpression slotBody = p._3();
        FreeExpression newBody = slotBody.lazyPartial(new BodyPartialResolver(resolver, name -> slotObjectRef.exists(name::equals)));
        return P.p(slotName, slotObjectRef, newBody);
    }

    @Override
    public Option<FreeExpression> partial(PartialResolver resolver) {
        List<P3<String, Option<String>, FreeExpression>> newSlots = slots.map(p -> partialBindSlot(p, resolver));
        if(ListUtil.elementsEq(newSlots, this.slots))
            return Option.none();
        return Option.some(new FreeObjectLiteral(ranges, newSlots));
    }

    public <T> P2<String, SlotInstance<T>> bindSlot(EvalContext<T> ctx, String slotName, Option<String> slotObjectRef, FreeExpression body,
        Resolver<T> resolver, InstanceAlgebra<T> algebra) {
        
        if(slotObjectRef.isNone()) {
            T t = body.eval(ctx, resolver, algebra);
            return P.p(slotName, SlotInstance.closed(t));
        } else {
            return P.p(slotName, SlotInstance.open(slotName, slotObjectRef.some(), body, resolver));
        }
    }

    @Override
    public <T> T eval(EvalContext<T> ctx, Resolver<T> resolver, InstanceAlgebra<T> algebra) {
        List<P2<String, SlotInstance<T>>> slots = this.slots
                .map(p -> bindSlot(ctx, p._1(), p._2(), p._3(), resolver, algebra));
        return algebra.slotMemoizer(algebra.objectLiteral(ranges, TreeMap.iterableTreeMap(Ord.stringOrd, slots)));
	}

	@Override
	public String toString() {
        return "{" + ListUtil.insertCommas(slots.map(p -> p._1() + "=...")) + "}";
	}

    @Override
    public <T> T acceptVisitor(FreeExpressionVisitor<T> visitor) {
        return visitor.objectLiteral(this);
    }
}