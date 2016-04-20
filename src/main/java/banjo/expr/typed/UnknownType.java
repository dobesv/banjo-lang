package banjo.expr.typed;

public class UnknownType implements ExprType {
    public static final UnknownType INSTANCE = new UnknownType();

    @Override
    public <T> T acceptVisitor(ExprTypeVisitor<T> visitor) {
        return visitor.unknown(this);
    }

}
