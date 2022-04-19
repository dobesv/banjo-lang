package banjo.eval.resolver;

import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public class EmptyResolver<T> implements Resolver<T> {
    final InstanceAlgebra<T> algebra;

    public EmptyResolver(InstanceAlgebra<T> algebra) {
        super();
        this.algebra = algebra;
    }

    @Override
    public T local(Set<SourceFileRange> ranges, String name) {
        return algebra.unboundIdentifier(ranges, name);
    }

    @Override
    public T slot(NameRef object, Set<SourceFileRange> ranges, String slotName) {
        return algebra.unboundIdentifier(ranges, slotName);
    }

    @Override
    public T baseSlot(Set<SourceFileRange> ranges, String slotObjectRef, String slotName) {
        return algebra.unboundIdentifier(ranges, slotName);
    }

    @Override
    public T functionBase(Set<SourceFileRange> ranges, String calleeName) {
        return algebra.unboundIdentifier(ranges, calleeName);
    }

    @Override
    public T invalid(Set<SourceFileRange> ranges, String reason) {
        return algebra.fail(ranges, reason);
    }
}