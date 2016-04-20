package banjo.expr.typed;

public class FunctionSelfTypeBinding implements TypeBinding {
    public final ExprType selfType;

    public FunctionSelfTypeBinding(ExprType selfType) {
        super();
        this.selfType = selfType;
    }

    @Override
    public <T> T acceptVisitor(TypeBindingVisitor<T> visitor) {
        return visitor.functionSelf(selfType);
    }

}
