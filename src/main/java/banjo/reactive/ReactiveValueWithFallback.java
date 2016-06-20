package banjo.reactive;

import banjo.value.Value;
import fj.data.Either;
import fj.data.List;
import fj.data.Option;

public class ReactiveValueWithFallback implements ReactiveValue {
    public final ReactiveValue base;
    public final Either<ReactiveValue, Value> alternative;

    public ReactiveValueWithFallback(ReactiveValue base, Either<ReactiveValue, Value> alternative) {
        this.base = base;
        this.alternative = alternative;
    }

    @Override
    public Option<Either<ReactiveValue, Value>> react(List<ReactiveValue> trace, Value value) {
        return base.react(trace, value).map(e -> e.left().map(rv -> rv.orElse(alternative)));
    }

    @Override
    public Value currentValue(List<ReactiveValue> trace) {
        return ReactiveValue.currentValue(trace, alternative);
    }

}
