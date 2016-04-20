package banjo.expr.typed;

public class KernelStringType extends KernelTypeWrapper implements ExprType {

    public final String string;

    public KernelStringType(String string, ExprType trueType, ExprType falseType) {
        super(trueType, falseType);
        this.string = string;
    }

    @Override
    public <T> T acceptVisitor(ExprTypeVisitor<T> visitor) {
        return visitor.kernelString(this);
    }

}
