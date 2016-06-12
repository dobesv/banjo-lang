package banjo.eval.expr;

public interface SlotInstanceVisitor<T, V> {

    V closed(ClosedSlotInstance<T> closedSlotInstance);

    V open(OpenSlotInstance<T> openSlotInstance);

}
