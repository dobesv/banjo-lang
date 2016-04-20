package banjo.expr.typed;

public class SlotSelfTypeBinding implements TypeBinding {
    public final ExprType selfType;
    public final String slotName;

    public SlotSelfTypeBinding(ExprType sourceObject, String slotName) {
        this.selfType = sourceObject;
        this.slotName = slotName;
    }

    @Override
    public <T> T acceptVisitor(TypeBindingVisitor<T> visitor) {
        return visitor.slot(selfType, slotName);
    }

}
