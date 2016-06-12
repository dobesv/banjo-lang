package banjo.eval.resolver;

import banjo.expr.util.SourceFileRange;
import fj.data.Option;
import fj.data.Set;
import fj.data.TreeMap;

public class ClosureResolver<T> implements Resolver<T> {
    public final TreeMap<NameRef, T> closure;
    public final InstanceAlgebra<T> algebra;
    public final NameRefAlgebra<Option<T>> locals;

    public ClosureResolver(TreeMap<NameRef, T> closure, InstanceAlgebra<T> algebra, NameRefAlgebra<Option<T>> locals) {
        this.closure = closure;
        this.algebra = algebra;
        this.locals = locals;
    }

    public T fromClosure(NameRef ref) {
        return closure.get(ref).orSome(() -> algebra.unboundIdentifier(ref.getRanges(), ref.toString()));
    }

    @Override
    public T local(Set<SourceFileRange> ranges, String name) {
        return locals.local(ranges, name).orSome(() -> fromClosure(NameRef.local(ranges, name)));
    }

    @Override
    public T slot(NameRef object, Set<SourceFileRange> ranges, String slotName) {
        return locals.slot(object, ranges, slotName).orSome(() -> fromClosure(NameRef.slot(object, ranges, slotName)));
    }

    @Override
    public T baseSlot(Set<SourceFileRange> ranges, String slotObjectRef, String slotName) {
        return locals.baseSlot(ranges, slotObjectRef, slotName).orSome(() -> fromClosure(NameRef.baseSlot(ranges, slotObjectRef, slotName)));
    }

    @Override
    public T functionBase(Set<SourceFileRange> ranges, String functionSelfName) {
        return locals.functionBase(ranges, functionSelfName).orSome(() -> fromClosure(NameRef.functionBase(ranges, functionSelfName)));
    }

    @Override
    public T invalid(Set<SourceFileRange> ranges, String reason) {
        return fromClosure(NameRef.invalid(ranges, reason));
    }

    @Override
    public T global(GlobalRef globalRef) {
        return fromClosure(globalRef);
    }
}
