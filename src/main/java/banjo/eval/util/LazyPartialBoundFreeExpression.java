package banjo.eval.util;

import banjo.eval.EvalContext;
import banjo.eval.resolver.InstanceAlgebra;
import banjo.eval.resolver.NameRef;
import banjo.eval.resolver.Resolver;
import banjo.expr.free.FreeExpression;
import banjo.expr.free.FreeExpressionVisitor;
import banjo.expr.free.PartialResolver;
import fj.data.Option;
import fj.data.Set;

/**
 * An expression plus an environment = a thunk
 */
public class LazyPartialBoundFreeExpression implements FreeExpression {
	public final FreeExpression expr;
    public final PartialResolver resolver;
    public FreeExpression memo;
	
    public LazyPartialBoundFreeExpression(FreeExpression expr, PartialResolver resolver) {
		super();
		this.expr = expr;
        this.resolver = resolver;
	}
	
    public FreeExpression get() {
        if(this.memo == null) {
            this.memo = this.calculate();
        }
        return this.memo;
    }

    public FreeExpression calculate() {
        return expr.partial(resolver).orSome(expr);
	}

    @Override
    public Set<NameRef> getFreeRefs() {
        return get().getFreeRefs();
    }

    @Override
    public boolean hasFreeRefs() {
        return get().hasFreeRefs();
    }

    @Override
    public Option<FreeExpression> partial(PartialResolver resolver) {
        return Option.some(lazyPartial(resolver));
    }

    @Override
    public <T> T eval(EvalContext<T> ctx, Resolver<T> resolver, InstanceAlgebra<T> algebra) {
        return get().eval(ctx, resolver, algebra);
    }

    @Override
    public FreeExpression lazyPartial(PartialResolver resolver) {
        return new LazyPartialBoundFreeExpression(expr, this.resolver.andThen(resolver));
    }

    @Override
    public String toString() {
        return get().toString();
    }

    @Override
    public <T> T acceptVisitor(FreeExpressionVisitor<T> visitor) {
        return get().acceptVisitor(visitor);
    }
}

