package banjo.reactive;

import banjo.eval.resolver.NameRef;
import banjo.eval.resolver.ValueInstanceAlgebra;
import banjo.expr.free.FreeExpression;
import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import fj.P;
import fj.data.Either;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.TreeMap;

public class ReactiveFunctionInstance implements ReactiveValue {

    public final Set<SourceFileRange> ranges;
    public final List<String> args;
    public final FreeExpression body;
    public final Option<String> calleeBinding;
    public final Either<ReactiveValue, Value> trait;
    public final TreeMap<NameRef, Either<ReactiveValue, Value>> closure;

    public ReactiveFunctionInstance(Set<SourceFileRange> ranges, List<String> args, FreeExpression body, Option<String> calleeBinding,
        Either<ReactiveValue, Value> trait, TreeMap<NameRef, Either<ReactiveValue, Value>> closure) {
        this.ranges = ranges;
        this.args = args;
        this.body = body;
        this.calleeBinding = calleeBinding;
        this.trait = trait;
        this.closure = closure;
    }

    @Override
    public Option<Either<ReactiveValue, Value>> react(List<ReactiveValue> trace, Value value) {
        Option<Either<ReactiveValue, Value>> traitReaction = ReactiveValue.reaction(trait, trace, value);
        TreeMap<NameRef, Option<Either<ReactiveValue, Value>>> closureReaction = closure.map(r -> ReactiveValue.reaction(r, trace, value));
        if(traitReaction.isNone() && closureReaction.values().forall(Option::isNone)) {
            return Option.none();
        }
        TreeMap<NameRef, Either<ReactiveValue, Value>> newClosure =
            TreeMap.iterableTreeMap(NameRef.ORD, closureReaction.toStream().bind(pp -> pp._2().map(v -> P.p(pp._1(), v)).toStream())).union(this.closure);
        Either<ReactiveValue, Value> newTrait = traitReaction.orSome(trait);
        return Option.some(ReactiveValueAlgebra.INSTANCE.functionInstance(ranges, args, body, calleeBinding, newTrait, newClosure));
    }

    @Override
    public Value currentValue(List<ReactiveValue> trace) {
        Value traitCurrentValue = ReactiveValue.currentValue(trace, trait);
        TreeMap<NameRef, Value> currentClosure = closure.map(v -> ReactiveValue.currentValue(trace, v));
        return ValueInstanceAlgebra.INSTANCE.functionInstance(ranges, args, body, calleeBinding, traitCurrentValue, currentClosure);
    }

}
