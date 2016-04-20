package banjo.expr.typed;

public interface TypeBindingVisitor<T> {

    T let(ExprType type);

    T functionSelf(ExprType selfType);

    T functionSelfWithBase(ExprType selfType, ExprType baseType);

    T slot(ExprType selfType, String slotName);

    T slotWithBase(ExprType selfType, String slotName, ExprType fallbackType);

}
