package banjo.expr.typed;

public class KernelNumberType extends KernelTypeWrapper implements ExprType {

    public final Number number;

    public KernelNumberType(Number number, ExprType trueType, ExprType falseType) {
        super(trueType, falseType);
        this.number = number;
    }

    @Override
    public <T> T acceptVisitor(ExprTypeVisitor<T> visitor) {
        return visitor.kernelNumber(this);
    }

}
