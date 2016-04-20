package banjo.expr.typed;

public class FunctionSelfWithBaseTypeBinding implements TypeBinding {
    public final ExprType selfType;
    public final ExprType baseType;

    public FunctionSelfWithBaseTypeBinding(ExprType selfType, ExprType baseType) {
        super();
        this.selfType = selfType;
        this.baseType = baseType;
    }

    @Override
    public <T> T acceptVisitor(TypeBindingVisitor<T> visitor) {
        return visitor.functionSelfWithBase(selfType, baseType);
    }

}
