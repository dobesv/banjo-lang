package banjo.expr.typed;

public abstract class KernelTypeWrapper implements ExprType {

    public final ExprType trueType;
    public final ExprType falseType;

    public KernelTypeWrapper(ExprType trueType, ExprType falseType) {
        super();
        this.trueType = trueType;
        this.falseType = falseType;
    }

}
