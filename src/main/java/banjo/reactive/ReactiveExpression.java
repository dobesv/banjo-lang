package banjo.reactive;

import banjo.expr.free.FreeExpression;
import banjo.value.Value;
import fj.data.Either;
import fj.data.List;
import fj.data.Option;
import fj.data.TreeMap;

public class ReactiveExpression implements ReactiveValue {
    public final FreeExpression expression;
    public final TreeMap<String, Either<ReactiveValue, Value>> arguments;
    
    public ReactiveExpression(FreeExpression expression, TreeMap<String, Either<ReactiveValue, Value>> arguments) {
        super();
        this.expression = expression;
        this.arguments = arguments;
    }

    @Override
    public Option<Either<ReactiveValue, Value>> react(List<ReactiveValue> trace, Value value) {
        // List<ReactiveValue> newTrace = trace.cons(this);
        //
        // argumentChanges = TreeMap.iterableTreeMap(Ord.stringOrd, arguments.toStream().bind(p -> ReactiveValue.reaction(p._2(), newTrace, value).map(newArg -> P.p(p._1(), newArg)).toStream()));
        //
        // TreeMap<String, P2<Either<ReactiveValue, Value>, Option<Either<ReactiveValue, Value>>>> argumentReactions =
        // arguments.map(arg -> P.p(arg, arg.either(rv -> rv.react(newTrace, value), v -> Option.none())));
        //
        // boolean argumentsUnchanged = argumentReactions.values().forall(p -> p._2().isNone());
        // if(argumentsUnchanged) {
        // // No change
        // return Option.none();
        // }
        // List<Either<ReactiveValue, Value>> newArguments =
        // arguments.
        // argumentsUnchanged ? arguments
        // : argumentReactions
        // .zipWith(arguments, Option::orSome);
        //
        // if(newArguments.forall(Either::isRight)) {
        //
        // // Not reactive any more
        // return Option.some(Either.right(calculateValue(
        // newTrace,
        // newArguments.map(e -> e.right().value()))));
        // }
        //
        // return Option.some(Either.left(new ReactiveExpression(expression, newArguments)));
        return null;
    }

    public Value calculateValue(List<ReactiveValue> trace, TreeMap<String, Value> arguments) {
        return null;

    }

    @Override
    public Value currentValue(List<ReactiveValue> trace) {
        return calculateValue(trace, arguments.map(e -> ReactiveValue.currentValue(trace, e)));
    }

}
