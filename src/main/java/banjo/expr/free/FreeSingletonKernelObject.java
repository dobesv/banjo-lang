package banjo.expr.free;

import banjo.eval.EvalContext;
import banjo.eval.resolver.InstanceAlgebra;
import banjo.eval.resolver.NameRef;
import banjo.eval.resolver.Resolver;
import fj.data.Option;
import fj.data.Set;

public class FreeSingletonKernelObject implements FreeExpression {
    public final String name;
    public final SingletonKernelObjectFactory f;

    public interface SingletonKernelObjectFactory {
        public <T> T apply(Resolver<T> resolver, InstanceAlgebra<T> algebra);

        public default Set<NameRef> getFreeRefs() {
            return NameRef.EMPTY_SET;
        }
    }

    public FreeSingletonKernelObject(String name, SingletonKernelObjectFactory f) {
        this.name = name;
        this.f = f;
    }

    @Override
    public Set<NameRef> getFreeRefs() {
        return f.getFreeRefs();
    }

    @Override
    public boolean hasFreeRefs() {
        return false;
    }

    @Override
    public Option<FreeExpression> partial(PartialResolver resolver) {
        return Option.none();
    }

    @Override
    public <T> T eval(EvalContext<T> ctx, Resolver<T> resolver, InstanceAlgebra<T> algebra) {
        return f.apply(resolver, algebra);
    }

    @Override
    public <T> T acceptVisitor(FreeExpressionVisitor<T> visitor) {
        return visitor.singletonKernelObject(this);
    }

}
