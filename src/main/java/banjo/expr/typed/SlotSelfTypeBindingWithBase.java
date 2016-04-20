package banjo.expr.typed;

public class SlotSelfTypeBindingWithBase implements TypeBinding {
    public final ExprType selfType;
    public final String slotName;
    public final ExprType fallbackType;

    public SlotSelfTypeBindingWithBase(ExprType selfType, String slotName, ExprType fallbackType) {
        this.selfType = selfType;
        this.slotName = slotName;
        this.fallbackType = fallbackType;
    }

    @Override
    public <T> T acceptVisitor(TypeBindingVisitor<T> visitor) {
        return visitor.slotWithBase(selfType, slotName, fallbackType);
    }

}
