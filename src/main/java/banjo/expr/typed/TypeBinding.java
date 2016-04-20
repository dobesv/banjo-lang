package banjo.expr.typed;

public interface TypeBinding {
    public <T> T acceptVisitor(TypeBindingVisitor<T> visitor);

    public static TypeBinding let(ExprType et) {
        return new LetTypeBinding(et);
    }

    public default ExprType getType() {
        return this.acceptVisitor(new TypeBindingVisitor<ExprType>() {
            @Override
            public ExprType let(ExprType type) {
                return type;
            }

            @Override
            public ExprType functionSelf(ExprType selfType) {
                return selfType;
            }

            @Override
            public ExprType functionSelfWithBase(ExprType selfType, ExprType baseType) {
                return selfType;
            }

            @Override
            public ExprType slot(ExprType selfType, String slotName) {
                return selfType;
            }

            @Override
            public ExprType slotWithBase(ExprType selfType, String slotName, ExprType fallbackType) {
                return selfType;
            }
        });
    }

    public static TypeBinding functionSelf(ExprType selfType) {
        return new FunctionSelfTypeBinding(selfType);
    }

    public static TypeBinding functionSelfWithBase(ExprType selfType, ExprType baseType) {
        return new FunctionSelfWithBaseTypeBinding(selfType, baseType);
    }

    public static TypeBinding slot(ExprType selfType, String slotName) {
        return new SlotSelfTypeBinding(selfType, slotName);
    }

    public static TypeBinding slotWithBase(ExprType selfType, String slotName, ExprType fallbackType) {
        return new SlotSelfTypeBindingWithBase(selfType, slotName, fallbackType);
    }
}
