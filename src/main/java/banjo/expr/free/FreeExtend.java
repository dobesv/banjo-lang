package banjo.expr.free;

import banjo.eval.EvalContext;
import banjo.eval.resolver.InstanceAlgebra;
import banjo.eval.resolver.NameRef;
import banjo.eval.resolver.Resolver;
import banjo.expr.source.Operator;
import fj.data.Option;
import fj.data.Set;

public class FreeExtend implements FreeExpression {
	public final FreeExpression base;
	public final FreeExpression extension;
	public FreeExtend(FreeExpression base, FreeExpression extension) {
        super();
        this.base = base;
        this.extension = extension;
    }

    @Override
    public Set<NameRef> getFreeRefs() {
        return base.getFreeRefs().union(extension.getFreeRefs());
    }

    @Override
    public boolean hasFreeRefs() {
        return base.hasFreeRefs() || extension.hasFreeRefs();
    }

    @Override
    public Option<FreeExpression> partial(PartialResolver lookupBinding) {
        Option<FreeExpression> newBase = base.partial(lookupBinding);
        Option<FreeExpression> newExtension = extension.partial(lookupBinding);
        if(newBase.isNone() && newExtension.isNone())
            return Option.none();
        return Option.some(new FreeExtend(newBase.orSome(base), newExtension.orSome(extension)));
    }

    @Override
    public <T> T eval(EvalContext<T> ctx, Resolver<T> resolver, InstanceAlgebra<T> algebra) {
        T b = base.eval(ctx, resolver, algebra);
        T e = extension.eval(ctx, resolver, algebra);
        return algebra.slotMemoizer(algebra.extend(b, e));
    }

	@Override
	public String toString() {
	    return base + " " + Operator.EXTENSION.getOp() + " " + extension;
	}

    @Override
    public <T> T acceptVisitor(FreeExpressionVisitor<T> visitor) {
        return visitor.extend(this);
    }
}