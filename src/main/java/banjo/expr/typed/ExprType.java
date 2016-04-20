package banjo.expr.typed;

import fj.data.List;

public interface ExprType {

    public <T> T acceptVisitor(ExprTypeVisitor<T> visitor);

    public default ExprType slot(List<ExprType> trace, String slotName, ExprType selfType, ExprType fallbackType) {
        return fallbackType;
    }

    public default ExprType call(List<ExprType> trace, List<ExprType> argTypes) {
        return UnknownType.INSTANCE;
    }

    public default ExprType call1(List<ExprType> trace, ExprType argument) {
        return call(trace, List.single(argument));
    }
}
