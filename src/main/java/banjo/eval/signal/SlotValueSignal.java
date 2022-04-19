package banjo.eval.signal;

import java.util.Arrays;

import banjo.expr.util.OrdUtil;
import banjo.expr.util.SourceFileRange;
import banjo.value.SlotValue;
import banjo.value.Value;
import fj.Ord;
import fj.data.Option;
import fj.data.Set;

public class SlotValueSignal implements Signal {
    public static final Ord<SlotValueSignal> ORD = OrdUtil.chain(SignalOrd.ORD.contramap(SlotValueSignal::getObject),
            Ord.stringOrd.contramap(SlotValueSignal::getSlotName),
            SignalOrd.ORD.contramap(SlotValueSignal::getOriginalObject),
            Ord.optionOrd(SignalOrd.ORD).contramap(SlotValueSignal::getPrevSlotValue));
    public final Signal object;
    public final Set<SourceFileRange> ranges;
    public final String slotName;
    public final Signal originalObject;
    public final Option<Signal> prevSlotValue;

    public SlotValueSignal(Signal object, Set<SourceFileRange> ranges, String slotName) {
        this(object, object, slotName, ranges, Option.none());
    }

    public SlotValueSignal(Signal object, Signal originalObject, String slotName, Set<SourceFileRange> ranges,
            Option<Signal> prevSlotValue) {
        this.object = object;
        this.originalObject = originalObject;
        this.ranges = ranges;
        this.slotName = slotName;
        this.prevSlotValue = prevSlotValue;
    }

    @Override
    public <T> T acceptVisitor(SignalVisitor<T> visitor) {
        return visitor.slotValue(this);
    }

    @Override
    public SignalNetwork network() {
        return object.connect(this).merge(originalObject.connect(this))
                .merge(prevSlotValue.map((psv) -> psv.connect(this)).orSome(SignalNetwork.EMPTY));
    }


    public SignalUpdate signalUpdate(SignalUpdate objectUpdate, SignalUpdate originalObjectUpdate,
            Option<SignalUpdate> prevSlotValueUpdate) {
        Iterable<SignalUpdate> sourceUpdates = prevSlotValueUpdate.option(
                () -> Arrays.<SignalUpdate>asList(objectUpdate, originalObjectUpdate),
                (psvu) -> Arrays.<SignalUpdate>asList(objectUpdate, originalObjectUpdate, psvu));
        return new SignalUpdate() {

            @Override
            public Value getValue(long t) {
                Value objectValue = objectUpdate.getValue(t);
                Value originalObjectValue = originalObjectUpdate.getValue(t);
                Option<Value> prevSlotValueValue = prevSlotValueUpdate.map((psvu) -> psvu.getValue(t));
                return new SlotValue(objectValue, originalObjectValue, slotName, ranges, prevSlotValueValue);
            }

            @Override
            public Iterable<SignalUpdate> getSourceUpdates() {
                return sourceUpdates;
            }
        };
    }

    private Option<SignalUpdate> update2(SignalUpdate objectUpdate, SignalUpdate originalObjectUpdate,
            SignalNetwork network) {
        // Cases:
        // 1. prevSlotValue.isNone() ? call signalUpdate with prevSlotValue =
        // none()
        // 2. prevSlotValue.isSome and prevSlotValue.update(network).isSome() ?
        // call signalUpdate with prevSlotValue = some()
        // 3. prevSlotValue.isSome and presSlotValue.update(network).isNone() ?
        // Return none()
        Option<Option<SignalUpdate>> t1 = prevSlotValue.option(Option.some(Option.none()),
                (psv) -> psv.update(network).map(Option::some));
        return t1.map(
                (prevSlotValueUpdate) -> this.signalUpdate(objectUpdate, originalObjectUpdate, prevSlotValueUpdate));
    }

    private Option<SignalUpdate> update1(SignalUpdate objectUpdate, SignalNetwork network) {
        // Cases:
        // 1. originalObject == object, call update2 directly
        // 2. Call originalObject.update() and call update2 with the SignalUpdate result
        if(originalObject != object) {
            return originalObject.update(network)
                    .bind((originalObjectUpdate) -> this.update2(objectUpdate, originalObjectUpdate, network));
        } else {
            return this.update2(objectUpdate, objectUpdate, network);
        }
    }

    @Override
    public Option<SignalUpdate> update(SignalNetwork network) {
        return object.update(network).bind((objectUpdate) -> this.update1(objectUpdate, network));
    }

    public Signal getObject() {
        return object;
    }

    public Set<SourceFileRange> getRanges() {
        return ranges;
    }

    public String getSlotName() {
        return slotName;
    }

    public Signal getOriginalObject() {
        return originalObject;
    }

    public Option<Signal> getPrevSlotValue() {
        return prevSlotValue;
    }

}
