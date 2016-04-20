package banjo.expr.typed;

public class KernelBooleanType extends KernelTypeWrapper {

    public final boolean value;

    public KernelBooleanType(boolean value, ExprType trueType, ExprType falseType) {
        super(trueType, falseType);
        this.value = value;
    }

    @Override
    public <T> T acceptVisitor(ExprTypeVisitor<T> visitor) {
        return visitor.kernelBoolean(this);
    }

}
