package banjo.reactive;

import banjo.eval.resolver.ValueInstanceAlgebra;
import banjo.value.Value;
import fj.data.Either;
import fj.data.List;
import fj.data.Option;

public class ReactiveExtend implements ReactiveValue {
    public final Either<ReactiveValue, Value> base;
    public final Either<ReactiveValue, Value> extension;

    public ReactiveExtend(Either<ReactiveValue, Value> base, Either<ReactiveValue, Value> extension) {
        super();
        this.base = base;
        this.extension = extension;
    }

    @Override
    public Option<Either<ReactiveValue, Value>> react(List<ReactiveValue> trace, Value value) {
        Option<Either<ReactiveValue, Value>> baseReaction = ReactiveValue.reaction(base, trace, value);
        Option<Either<ReactiveValue, Value>> extensionReaction = ReactiveValue.reaction(extension, trace, value);
        if(baseReaction.isNone() && extensionReaction.isNone())
            return Option.none();
        Either<ReactiveValue, Value> newBase = baseReaction.orSome(base);
        Either<ReactiveValue, Value> newExtension = extensionReaction.orSome(extension);
        return Option.some(ReactiveValueAlgebra.INSTANCE.extend(newBase, newExtension));
    }

    @Override
    public Value currentValue(List<ReactiveValue> trace) {
        Value baseCurrentValue = ReactiveValue.currentValue(trace, base);
        Value extensionCurrentValue = ReactiveValue.currentValue(trace, extension);
        return ValueInstanceAlgebra.INSTANCE.extend(baseCurrentValue, extensionCurrentValue);
    }

}
