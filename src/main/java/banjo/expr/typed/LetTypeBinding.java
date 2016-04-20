package banjo.expr.typed;

public class LetTypeBinding implements TypeBinding {
    public final ExprType type;

    public LetTypeBinding(ExprType type) {
        super();
        this.type = type;
    }

    @Override
    public <T> T acceptVisitor(TypeBindingVisitor<T> visitor) {
        return visitor.let(type);
    }

}
