package banjo.expr.typed;

import fj.data.List;

public class SlotType implements ExprType {
    public final ExprType objectType;
    public final ExprType selfType;
    public final ExprType fallbackType;
    public final String slotName;
    private ExprType memo;

    public SlotType(ExprType objectType, ExprType selfType, String slotName, ExprType fallback) {
        super();
        this.objectType = objectType;
        this.selfType = selfType;
        this.fallbackType = fallback;
        this.slotName = slotName;
    }

    public ExprType force(List<ExprType> trace) {
        if(memo == null) {
            memo = calculate(trace);
        }
        return memo;
    }

    public ExprType calculate(List<ExprType> trace) {
        return objectType.slot(trace.cons(this), slotName, selfType, fallbackType);
    }

    @Override
    public <T> T acceptVisitor(ExprTypeVisitor<T> visitor) {
        return force(List.nil()).acceptVisitor(visitor);
    }

}
