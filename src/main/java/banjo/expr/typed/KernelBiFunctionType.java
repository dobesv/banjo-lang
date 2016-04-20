package banjo.expr.typed;

public class KernelBiFunctionType implements ExprType {

    @Override
    public <T> T acceptVisitor(ExprTypeVisitor<T> visitor) {
        return visitor.kernelBiFunction(this);
    }

}
