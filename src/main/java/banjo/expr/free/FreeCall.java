package banjo.expr.free;

import banjo.eval.resolver.InstanceAlgebra;
import banjo.eval.resolver.NameRef;
import banjo.eval.resolver.Resolver;
import banjo.expr.util.ListUtil;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;

public class FreeCall implements FreeExpression {
	public final Set<SourceFileRange> ranges;
	public final FreeExpression callee;
	public final List<FreeExpression> args;

	public FreeCall(Set<SourceFileRange> ranges, FreeExpression callee,
            List<FreeExpression> args) {
        super();
        this.ranges = ranges;
        this.callee = callee;
        this.args = args;
    }

    @Override
    public Set<NameRef> getFreeRefs() {
        Set<NameRef> argsFreeRefs = Set.join(NameRef.ORD, Set.iterableSet(Ord.setOrd(NameRef.ORD), args.map(FreeExpression::getFreeRefs)));
        Set<NameRef> functionFreeRefs = callee.getFreeRefs();
        return argsFreeRefs.union(functionFreeRefs);
    }

    @Override
    public boolean hasFreeRefs() {
        if(callee.hasFreeRefs())
            return true;
        for(FreeExpression arg : args) {
            if(arg.hasFreeRefs())
                return true;
        }
        return false;
    }

    @Override
    public Option<FreeExpression> partial(PartialResolver lookupBinding) {
        Option<FreeExpression> newCallee = callee.partial(lookupBinding);
        List<Option<FreeExpression>> newArgs = args.map(arg -> arg.partial(lookupBinding));
        if(newCallee.isNone() && !newArgs.exists(Option::isSome))
            return Option.none();
        // TODO If the callee and args all have no free refs, we could expand
        // the call right now
        List<FreeExpression> actualNewArgs = args.zipWith(newArgs, (oldArg, newArgOpt) -> newArgOpt.orSome(oldArg));
        FreeExpression actualNewCallee = newCallee.orSome(callee);
        return Option.some(FreeExpression.call(ranges, actualNewCallee, actualNewArgs));
    }

    /**
     * Bind the call to the environment, but do not evaluate it yet.
     */
    @Override
    public <T> T eval(List<T> trace, Resolver<T> resolver, InstanceAlgebra<T> algebra) {
        T callee = this.callee.eval(trace, resolver, algebra);
        List<T> args = this.args.map(arg -> arg.eval(trace, resolver, algebra));
        return algebra.call(trace, ranges, callee, args);
    }

	@Override
	public String toString() {
	    return callee + "(" + ListUtil.insertCommas(args) + ")";
	}

    @Override
    public <T> T acceptVisitor(FreeExpressionVisitor<T> visitor) {
        return visitor.call(this);
    }
}