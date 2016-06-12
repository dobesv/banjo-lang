package banjo.eval.resolver;

import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public abstract class WrappedResolver<T> implements Resolver<T> {
    protected final Resolver<T> delegate;

    public WrappedResolver(Resolver<T> delegate) {
        super();
        this.delegate = delegate;
    }

    @Override
    public T local(Set<SourceFileRange> ranges, String name) {
        return delegate.local(ranges, name);
    }

    @Override
    public T slot(NameRef object, Set<SourceFileRange> ranges, String slotName) {
        return delegate.slot(object, ranges, slotName);
    }

    @Override
    public T baseSlot(Set<SourceFileRange> ranges, String slotObjectRef, String slotName) {
        return delegate.baseSlot(ranges, slotObjectRef, slotName);
    }

    @Override
    public T functionBase(Set<SourceFileRange> ranges, String functionSelfName) {
        return delegate.functionBase(ranges, functionSelfName);
    }

    @Override
    public T invalid(Set<SourceFileRange> ranges, String reason) {
        return delegate.invalid(ranges, reason);
    }

    @Override
    public T global(GlobalRef globalRef) {
        return delegate.global(globalRef);
    }
}
