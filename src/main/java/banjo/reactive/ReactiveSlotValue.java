package banjo.reactive;

import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import fj.data.Either;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;

public class ReactiveSlotValue extends CalculatedReactiveValue implements ReactiveValue {
    public final Either<ReactiveValue, Value> object;
    public final String slotName;
    public final Set<SourceFileRange> ranges;
    public final Either<ReactiveValue, Value> originalObject;
    public final Either<ReactiveValue, Value> prevSlotValue;

    public ReactiveSlotValue(ReactiveValue object, String slotName, Set<SourceFileRange> ranges) {
        this.object = Either.left(object);
        this.originalObject = this.object;
        this.slotName = slotName;
        this.ranges = ranges;
        this.prevSlotValue = null;
    }

    public ReactiveSlotValue(Either<ReactiveValue, Value> object, Either<ReactiveValue, Value> originalObject, String slotName, Set<SourceFileRange> ranges,
        Either<ReactiveValue, Value> prevSlotValue) {
        this.object = object;
        this.originalObject = originalObject;
        this.slotName = slotName;
        this.ranges = ranges;
        this.prevSlotValue = prevSlotValue;
    }

    @Override
    public Option<Either<ReactiveValue, Value>> react(List<ReactiveValue> trace, Value value) {
        List<ReactiveValue> newTrace = trace.cons(this);
        Option<Either<ReactiveValue, Value>> objectReaction = ReactiveValue.reaction(object, newTrace, value);
        Option<Either<ReactiveValue, Value>> originalObjectReaction = ReactiveValue.reaction(originalObject, newTrace, value);
        Option<Either<ReactiveValue, Value>> prevSlotValueReaction = ReactiveValue.reaction(prevSlotValue, newTrace, value);
        if(objectReaction.isNone() && originalObjectReaction.isNone() && prevSlotValueReaction.isNone())
            return Option.none();
        Either<ReactiveValue, Value> newObject = objectReaction.orSome(object);
        Either<ReactiveValue, Value> newOriginalObject = originalObjectReaction.orSome(originalObject);
        Either<ReactiveValue, Value> newPrevSlotValue = prevSlotValueReaction.orSome(prevSlotValue);
        return Option.some(ReactiveValueAlgebra.INSTANCE.slotValue(newObject, newOriginalObject, slotName, ranges, newPrevSlotValue));
    }

    @Override
    public Value calculateCurrentValue(List<ReactiveValue> trace) {
        Value objectCurrentValue = ReactiveValue.currentValue(trace, object);
        Value originalObjectCurrentValue = ReactiveValue.currentValue(trace, originalObject);
        Value prevSlotCurrentValue = ReactiveValue.currentValue(trace, prevSlotValue);
        return objectCurrentValue.slot(
            trace.map(rv -> rv.currentValue(trace)),
            originalObjectCurrentValue,
            slotName,
            ranges,
            prevSlotCurrentValue);
    }
}
