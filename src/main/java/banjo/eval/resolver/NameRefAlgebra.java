package banjo.eval.resolver;

import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public interface NameRefAlgebra<T> {
    public default T visit(NameRef r) {
        return r.acceptVisitor(this);
    }

    T local(Set<SourceFileRange> ranges, String name);

    T slot(NameRef object, Set<SourceFileRange> ranges, String slotName);

    T baseSlot(Set<SourceFileRange> ranges, String slotObjectRef, String slotName);

    T functionBase(Set<SourceFileRange> ranges, String calleeName);

    T invalid(Set<SourceFileRange> ranges, String reason);

}
