package banjo.expr.free;

import banjo.eval.resolver.InstanceAlgebra;
import banjo.eval.resolver.NameRef;
import banjo.eval.resolver.Resolver;
import banjo.eval.resolver.WrappedResolver;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public class ProjectionResolver<T> extends WrappedResolver<T> {
    public final T object;
    public final InstanceAlgebra<T> algebra;

    public ProjectionResolver(Resolver<T> delegate, InstanceAlgebra<T> algebra, T object) {
        super(delegate);
        this.object = object;
        this.algebra = algebra;
    }

    @Override
    public T local(Set<SourceFileRange> ranges, String name) {
        return algebra.slotValue(object, ranges, name);
    }

    @Override
    public T slot(NameRef object, Set<SourceFileRange> ranges, String slotName) {
        return algebra.slotValue(object.acceptVisitor(this), ranges, slotName);
    }

    @Override
    public T baseSlot(Set<SourceFileRange> ranges, String slotObjectRef, String slotName) {
        return delegate.invalid(ranges, Identifier.toSource(slotObjectRef) + " is not a slot object-ref here");
    }

    @Override
    public T functionBase(Set<SourceFileRange> ranges, String functionSelfName) {
        return delegate.invalid(ranges, Identifier.toSource(functionSelfName) + " is not a function self-name here");
    }
}