package banjo.eval.expr;

import banjo.eval.EvalContext;
import banjo.eval.resolver.InstanceAlgebra;
import banjo.eval.resolver.NameRef;
import banjo.eval.resolver.Resolver;
import banjo.expr.free.FreeExpression;
import banjo.expr.free.FreeObjectLiteral;
import fj.Ord;
import fj.Ordering;
import fj.data.Option;
import fj.data.Set;

public interface SlotInstance<T> {
    public static <T> Ord<SlotInstance<T>> ord(Ord<T> ord) {
        return Ord.ord((SlotInstance<T> a) -> (SlotInstance<T> b) -> a == b ? Ordering.EQ : a.acceptVisitor(new SlotInstanceVisitor<T, Ordering>() {

            @Override
            public Ordering closed(ClosedSlotInstance<T> slot1) {
                return b.acceptVisitor(new SlotInstanceVisitor<T, Ordering>() {

                    @Override
                    public Ordering closed(ClosedSlotInstance<T> slot2) {
                        return ord.compare(slot1.value, slot2.value);
                    }

                    @Override
                    public Ordering open(OpenSlotInstance<T> openSlotInstance) {
                        return Ordering.LT;
                    }

                });
            }

            @Override
            public Ordering open(OpenSlotInstance<T> slot1) {
                return b.acceptVisitor(new SlotInstanceVisitor<T, Ordering>() {

                    @Override
                    public Ordering closed(ClosedSlotInstance<T> closedSlotInstance) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering open(OpenSlotInstance<T> slot2) {
                        return OpenSlotInstance.ord(ord).compare(slot1, slot2);
                    }

                });
            }

        }));
    }
    public static <T> ClosedSlotInstance<T> closed(T value) {
        return new ClosedSlotInstance<T>(value);
    }

    public static <T> SlotInstance<T> open(String slotName, String slotObjectRef, FreeExpression body, Resolver<T> resolver) {
        Set<NameRef> refs = body.getFreeRefs().filter(ref -> FreeObjectLiteral.isFree(slotObjectRef, ref));
        return new OpenSlotInstance<T>(slotName, slotObjectRef, body, resolver.closure(refs));
    }

    /**
     * Calculate the slot
     */
    public T apply(EvalContext<T> ctx, T self, Option<T> prevSlotValue, InstanceAlgebra<T> algebra);

    /**
     * Allow a calculation based on what type of slot this is - open or closed.
     */
    public <V> V acceptVisitor(SlotInstanceVisitor<T, V> visitor);

}
