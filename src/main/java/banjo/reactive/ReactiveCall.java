package banjo.reactive;

import banjo.value.Value;
import fj.data.Either;
import fj.data.List;
import fj.data.Option;

public class ReactiveCall extends CalculatedReactiveValue implements ReactiveValue {

    public final Either<ReactiveValue, Value> callee;
    public final Either<ReactiveValue, Value> originalCallee;
    public final Either<ReactiveValue, Value> baseCallable;
    public final List<Either<ReactiveValue, Value>> arguments;

    public ReactiveCall(Either<ReactiveValue, Value> callee, Either<ReactiveValue, Value> originalCallee, Either<ReactiveValue, Value> baseCallable,
        List<Either<ReactiveValue, Value>> arguments) {
        this.callee = callee;
        this.originalCallee = originalCallee;
        this.baseCallable = baseCallable;
        this.arguments = arguments;
    }

    @Override
    public Option<Either<ReactiveValue, Value>> react(List<ReactiveValue> trace, Value value) {
        List<ReactiveValue> newTrace = trace.cons(this);
        Option<Either<ReactiveValue, Value>> calleeReaction = ReactiveValue.reaction(callee, newTrace, value);
        Option<Either<ReactiveValue, Value>> originalCalleeReaction = ReactiveValue.reaction(originalCallee, newTrace, value);
        Option<Either<ReactiveValue, Value>> baseCallableReaction = ReactiveValue.reaction(baseCallable, newTrace, value);
        List<Option<Either<ReactiveValue, Value>>> argumentReactions = arguments.map(arg -> arg.either(rv -> rv.react(newTrace, value), v -> Option.none()));
        boolean argumentsUnchanged = argumentReactions.forall(Option::isNone);
        if(calleeReaction.isNone() && 
            originalCalleeReaction.isNone() &&
            baseCallableReaction.isNone() &&
            argumentsUnchanged) {
            // No change
            return Option.none();
        }
        Either<ReactiveValue, Value> newCallee = calleeReaction.orSome(callee);
        Either<ReactiveValue, Value> newOriginalCallee = originalCalleeReaction.orSome(originalCallee);
        Either<ReactiveValue, Value> newBaseCallable = baseCallableReaction.orSome(baseCallable);
        List<Either<ReactiveValue, Value>> newArguments =
            argumentsUnchanged ? arguments
                : argumentReactions
                    .zipWith(arguments, (ar, a) -> ar.orSome(a));
        
        if(newCallee.isRight() &&
            newOriginalCallee.isRight() &&
            (newBaseCallable == null || newBaseCallable.isRight()) &&
            newArguments.forall(Either::isRight)) {

            // Not reactive any more
            return Option.some(Either.right(calculateValue(
                trace,
                newCallee.right().value(),
                newOriginalCallee.right().value(),
                newBaseCallable == null ? null : newBaseCallable.right().value(),
                newArguments.map(e -> e.right().value()))));
        }
        
        return Option.some(Either.left(new ReactiveCall(newCallee, newOriginalCallee, newBaseCallable, newArguments)));
    }

    public Value calculateValue(List<ReactiveValue> trace, Value calleeValue, Value originalCalleeValue, Value baseCallableValue, List<Value> argumentValues) {
        return calleeValue.call(
            trace.map(rv -> rv.currentValue(trace)),
            originalCalleeValue,
            baseCallableValue,
            argumentValues);
    }

    @Override
    public Value calculateCurrentValue(List<ReactiveValue> trace) {
        return calculateValue(
            trace,
            ReactiveValue.currentValue(trace, callee),
            ReactiveValue.currentValue(trace, originalCallee),
            ReactiveValue.currentValue(trace, baseCallable),
            arguments.map(e -> ReactiveValue.currentValue(trace, e)));
    }

}
