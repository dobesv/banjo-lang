package banjo.eval.resolver;

import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public interface GlobalResolver<T> extends Resolver<T> {
    T getProjectRoot();

    InstanceAlgebra<T> getInstanceAlgebra();

    @Override
    public default T local(Set<SourceFileRange> ranges, String name) {
        return getInstanceAlgebra().slotValue(getProjectRoot(), ranges, name);
    }

    public default T global(String name) {
        return getInstanceAlgebra().slotValue(getProjectRoot(), SourceFileRange.EMPTY_SET, name);
    }

    @Override
    default T slot(NameRef object, Set<SourceFileRange> ranges, String slotName) {
        return getInstanceAlgebra().slotValue(apply(object), ranges, slotName);
    }

    @Override
    default T global(GlobalRef globalRef) {
        return global(globalRef.name);
    }
    @Override
    default T functionBase(Set<SourceFileRange> ranges, String functionSelfName) {
        return getInstanceAlgebra().unboundIdentifier(ranges, functionSelfName);
    }

    @Override
    default T baseSlot(Set<SourceFileRange> ranges, String slotObjectRef, String slotName) {
        return getInstanceAlgebra().unboundIdentifier(ranges, slotObjectRef);
    }

    @Override
    default T invalid(Set<SourceFileRange> ranges, String reason) {
        return getInstanceAlgebra().fail(ranges, reason);
    }


}
