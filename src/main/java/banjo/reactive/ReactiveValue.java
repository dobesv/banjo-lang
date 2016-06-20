package banjo.reactive;

import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import fj.Function;
import fj.data.Either;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;

/**
 * A FutureValue is a mechanism for reactivity. Initially the FutureValue has no value, or some fallback value. Later, the value is set procedurally by a runtime component or in reaction to a change
 * to another FutureValue.
 * <p>
 * Derived FutureValue instances recalculate their value when their source value(s) change.
 * 
 */
public interface ReactiveValue {

    /**
     * The source value has changed. This returns either:
     * <p>
     * <ul>
     * <li><code>Option.none()</code> to indicate the received value has no effect</li>
     * <li><code>Option.some(Either.left(...))</code> to indicate the value has changed, and may change again.</li>
     * <li><code>Option.some(Either.right(...))</code> to indicate the value has changed and will not change again.</li>
     * </ul>
     */
    public Option<Either<ReactiveValue, Value>> react(List<ReactiveValue> trace, Value value);

    public default ReactiveValue orElse(Either<ReactiveValue, Value> alternative) {
        return new ReactiveValueWithFallback(this, alternative);
    }

    public default ReactiveValue slot(String name, Set<SourceFileRange> ranges) {
        return new ReactiveSlotValue(this, name, ranges);
    }

    public default ReactiveValue call(
        Either<ReactiveValue, Value> originalCallee,
        Either<ReactiveValue, Value> baseCallable,
        List<Either<ReactiveValue, Value>> arguments) {
        return new ReactiveCall(Either.left(this), originalCallee, baseCallable, arguments);
    }

    /**
     * Current value; this value is used for the value of this until the source value is set.
     */
    public Value currentValue(List<ReactiveValue> trace);

    public static Option<Either<ReactiveValue, Value>> reaction(Either<ReactiveValue, Value> erv, List<ReactiveValue> trace, Value value) {
        if(erv == null)
            return Option.none();
        return erv.either(rv -> rv.react(trace, value), v -> Option.none());
    }

    public static Value currentValue(List<ReactiveValue> trace, Either<ReactiveValue, Value> e) {
        if(e == null)
            return null;
        return e.either(rv -> rv.currentValue(trace), Function.identity());
    }
}
