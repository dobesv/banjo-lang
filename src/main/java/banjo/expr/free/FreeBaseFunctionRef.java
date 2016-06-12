package banjo.expr.free;

import banjo.eval.resolver.InstanceAlgebra;
import banjo.eval.resolver.NameRef;
import banjo.eval.resolver.Resolver;
import banjo.expr.util.SourceFileRange;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;

public class FreeBaseFunctionRef implements FreeExpression {
	public final String name;
    public final Set<SourceFileRange> ranges;

    public FreeBaseFunctionRef(String name, Set<SourceFileRange> ranges) {
        super();
        this.name = name;
        this.ranges = ranges;
    }

    @Override
    public Set<NameRef> getFreeRefs() {
        return NameRef.EMPTY_SET.insert(NameRef.functionBase(ranges, name));
    }

    @Override
    public boolean hasFreeRefs() {
        return true;
    }

    @Override
    public Option<FreeExpression> partial(PartialResolver resolver) {
        // Replace this FreeExpression with one we get by looking up the
        // function base, if any
        return resolver.functionBase(ranges, name);
    }

    @Override
    public <T> T eval(List<T> trace, Resolver<T> resolver, InstanceAlgebra<T> algebra) {
        return resolver.functionBase(ranges, name);
    }

    @Override
    public <T> T acceptVisitor(FreeExpressionVisitor<T> visitor) {
        return visitor.baseFunctionRef(this);
    }
}