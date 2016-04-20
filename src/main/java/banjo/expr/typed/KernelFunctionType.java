package banjo.expr.typed;

public class KernelFunctionType implements ExprType {

    @Override
    public <T> T acceptVisitor(ExprTypeVisitor<T> visitor) {
        return visitor.kernelFunction(this);
    }

}
