package banjo.expr.typed;

import fj.data.List;

public class ExtendedObjectType implements ExprType {
    public final ExprType base;
    public final ExprType extension;

    public ExtendedObjectType(ExprType base, ExprType extension) {
        super();
        this.base = base;
        this.extension = extension;
    }

    @Override
    public <T> T acceptVisitor(ExprTypeVisitor<T> visitor) {
        return visitor.extendedObject(this);
    }

    @Override
    public ExprType slot(List<ExprType> trace, String slotName, ExprType selfType, ExprType fallbackType) {
        ExprType baseSlotType = base.slot(trace, slotName, selfType, fallbackType);
        return extension.slot(trace, slotName, selfType, baseSlotType);
    }
}
