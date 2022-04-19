package banjo.expr.free;

import banjo.eval.EvalContext;
import banjo.eval.resolver.InstanceAlgebra;
import banjo.eval.resolver.NameRef;
import banjo.eval.resolver.Resolver;
import banjo.expr.util.SourceFileRange;
import fj.data.Option;
import fj.data.Set;

public class FreeBadExpr implements FreeExpression {
    public final Set<SourceFileRange> ranges;
    public final String message;

    public FreeBadExpr(Set<SourceFileRange> ranges, String message) {
        super();
        this.ranges = ranges;
        this.message = message;
    }

    @Override
    public Set<NameRef> getFreeRefs() {
        return NameRef.EMPTY_SET;
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
        return algebra.badExpr(ranges, message);
    }

    @Override
    public <T> T acceptVisitor(FreeExpressionVisitor<T> visitor) {
        return visitor.badExpr(this);
    }

}
