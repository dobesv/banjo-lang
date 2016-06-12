package banjo.eval.resolver;

import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public class BaseSlotNameRef implements NameRef {

    public final Set<SourceFileRange> ranges;
    public final String slotObjectRef;
    public final String slotName;

    public BaseSlotNameRef(Set<SourceFileRange> ranges, String slotObjectName, String slotName) {
        this.ranges = ranges;
        this.slotObjectRef = slotObjectName;
        this.slotName = slotName;
    }

    @Override
    public <T> T acceptVisitor(NameRefAlgebra<T> visitor) {
        return visitor.baseSlot(ranges, slotObjectRef, slotName);
    }

    @Override
    public String toString() {
        return slotObjectRef + ":" + slotName;
    }

    @Override
    public Set<SourceFileRange> getRanges() {
        return ranges;
    }

    public String getSlotObjectName() {
        return slotObjectRef;
    }

    public String getSlotName() {
        return slotName;
    }

}
