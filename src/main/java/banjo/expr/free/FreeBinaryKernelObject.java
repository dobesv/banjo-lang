package banjo.expr.free;

import banjo.eval.resolver.InstanceAlgebra;
import banjo.eval.resolver.NameRef;
import banjo.eval.resolver.Resolver;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;

public class FreeBinaryKernelObject implements FreeExpression {
    public final FreeExpression a;
    public final FreeExpression b;
    public final String name;
    public final BinaryKernelObjectFactory f;

    public interface BinaryKernelObjectFactory {
        public <T> T apply(T a, T b, Resolver<T> resolver);
    }

    public FreeBinaryKernelObject(FreeExpression a, FreeExpression b, String name, BinaryKernelObjectFactory f) {
        super();
        this.a = a;
        this.b = b;
        this.name = name;
        this.f = f;
    }

    @Override
    public Set<NameRef> getFreeRefs() {
        return a.getFreeRefs().union(b.getFreeRefs());
    }

    @Override
    public boolean hasFreeRefs() {
        return a.hasFreeRefs() || b.hasFreeRefs();
    }

    @Override
    public Option<FreeExpression> partial(PartialResolver resolver) {
        Option<FreeExpression> aUpdate = a.partial(resolver);
        Option<FreeExpression> bUpdate = b.partial(resolver);
        if(aUpdate.isNone() && bUpdate.isNone())
            return Option.none();
        return Option.some(new FreeBinaryKernelObject(aUpdate.orSome(a), bUpdate.orSome(b), name, f));
    }

    @Override
    public <T> T eval(List<T> trace, Resolver<T> resolver, InstanceAlgebra<T> algebra) {
        return f.apply(a.eval(trace, resolver, algebra), b.eval(trace, resolver, algebra), resolver);
    }

    @Override
    public <T> T acceptVisitor(FreeExpressionVisitor<T> visitor) {
        return visitor.binaryKernelObject(this);
    }

}
