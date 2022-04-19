package banjo.expr.free;

import banjo.eval.EvalContext;
import banjo.eval.resolver.InstanceAlgebra;
import banjo.eval.resolver.NameRef;
import banjo.eval.resolver.Resolver;
import fj.data.Option;
import fj.data.Set;

public class FreeUnaryKernelObject implements FreeExpression {
    public final FreeExpression arg;
    public final String name;
    public final UnaryKernelObjectFactory f;

    public interface UnaryKernelObjectFactory {
        public <T> T apply(T arg, Resolver<T> resolver);
    }

    public FreeUnaryKernelObject(FreeExpression a, String name, UnaryKernelObjectFactory f) {
        super();
        this.arg = a;
        this.name = name;
        this.f = f;
    }

    @Override
    public Set<NameRef> getFreeRefs() {
        return arg.getFreeRefs();
    }

    @Override
    public boolean hasFreeRefs() {
        return arg.hasFreeRefs();
    }

    @Override
    public Option<FreeExpression> partial(PartialResolver resolver) {
        Option<FreeExpression> aUpdate = arg.partial(resolver);
        if(aUpdate.isNone())
            return Option.none();
        return Option.some(new FreeUnaryKernelObject(aUpdate.some(), name, f));
    }

    @Override
    public <T> T eval(EvalContext<T> ctx, Resolver<T> resolver, InstanceAlgebra<T> algebra) {
        return f.apply(arg.eval(ctx, resolver, algebra), resolver);
    }

    @Override
    public <T> T acceptVisitor(FreeExpressionVisitor<T> visitor) {
        return visitor.unaryKernelObject(this);
    }
}
