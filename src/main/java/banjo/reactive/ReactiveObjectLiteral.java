package banjo.reactive;

import banjo.eval.expr.ClosedSlotInstance;
import banjo.eval.expr.OpenSlotInstance;
import banjo.eval.expr.SlotInstance;
import banjo.eval.expr.SlotInstanceVisitor;
import banjo.eval.resolver.NameRef;
import banjo.eval.resolver.ValueInstanceAlgebra;
import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.P3;
import fj.data.Either;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.Stream;
import fj.data.TreeMap;

public class ReactiveObjectLiteral implements ReactiveValue {

    public final Set<SourceFileRange> ranges;
    public final TreeMap<String, SlotInstance<Either<ReactiveValue, Value>>> slots;

    public ReactiveObjectLiteral(Set<SourceFileRange> ranges, TreeMap<String, SlotInstance<Either<ReactiveValue, Value>>> slots) {
        this.ranges = ranges;
        this.slots = slots;
    }

    @Override
    public Option<Either<ReactiveValue, Value>> react(List<ReactiveValue> trace, Value value) {
        Stream<P3<String, SlotInstance<Either<ReactiveValue, Value>>, SlotInstance<Option<Either<ReactiveValue, Value>>>>> slotReactions =
            slots.toStream().map(p -> P.p(p._1(), p._2(), p._2().acceptVisitor(new SlotInstanceVisitor<Either<ReactiveValue, Value>, SlotInstance<Option<Either<ReactiveValue, Value>>>>() {

                @Override
                public SlotInstance<Option<Either<ReactiveValue, Value>>> closed(ClosedSlotInstance<Either<ReactiveValue, Value>> s) {
                    return new ClosedSlotInstance<Option<Either<ReactiveValue, Value>>>(ReactiveValue.reaction(s.value, trace, value));
                }

                @Override
                public SlotInstance<Option<Either<ReactiveValue, Value>>> open(OpenSlotInstance<Either<ReactiveValue, Value>> s) {
                    return new OpenSlotInstance<Option<Either<ReactiveValue, Value>>>(s.name, s.slotObjectRef, s.body,
                        s.closure.map(e -> ReactiveValue.reaction(e, trace, value)));
                }
                    })));

        // Check if there was no change
        if(slotReactions.forall(p -> p._3().acceptVisitor(new SlotInstanceVisitor<Option<Either<ReactiveValue, Value>>, Boolean>() {

            @Override
            public Boolean closed(ClosedSlotInstance<Option<Either<ReactiveValue, Value>>> s) {
                return s.value.isNone();
            }

            @Override
            public Boolean open(OpenSlotInstance<Option<Either<ReactiveValue, Value>>> s) {
                return s.closure.values().forall(Option::isNone);
            }

        }))) {
            return Option.none();
        }
        
        TreeMap<String, SlotInstance<Either<ReactiveValue, Value>>> newSlots =
            TreeMap.iterableTreeMap(
                Ord.stringOrd,
                slotReactions
                    .map(
                        p -> P.p(
                            p._1(),
                            p._3().acceptVisitor(new SlotInstanceVisitor<Option<Either<ReactiveValue, Value>>, SlotInstance<Either<ReactiveValue, Value>>>() {

                                @Override
                                public SlotInstance<Either<ReactiveValue, Value>> closed(ClosedSlotInstance<Option<Either<ReactiveValue, Value>>> s) {
                                    return new ClosedSlotInstance<Either<ReactiveValue, Value>>(
                                        s.value.orSome(((ClosedSlotInstance<Either<ReactiveValue, Value>>) p._2()).value));
                                }

                                @Override
                                public SlotInstance<Either<ReactiveValue, Value>> open(OpenSlotInstance<Option<Either<ReactiveValue, Value>>> s) {
                                    TreeMap<NameRef, Either<ReactiveValue, Value>> oldClosure =
                                        ((OpenSlotInstance<Either<ReactiveValue, Value>>) p._2()).closure;
                                    TreeMap<NameRef, Either<ReactiveValue, Value>> newClosure = TreeMap.iterableTreeMap(NameRef.ORD, s.closure.toStream().bind(pp -> pp._2().map(v -> P.p(pp._1(), v)).toStream())).union(oldClosure);
                                    return new OpenSlotInstance<Either<ReactiveValue, Value>>(s.name, s.slotObjectRef, s.body,
                                        newClosure);
                                }
                            }))));
        return Option.some(ReactiveValueAlgebra.INSTANCE.objectLiteral(ranges, newSlots));
    }

    @Override
    public Value currentValue(List<ReactiveValue> trace) {
        return ValueInstanceAlgebra.INSTANCE
            .objectLiteral(ranges, slots.map(si -> si.acceptVisitor(new SlotInstanceVisitor<Either<ReactiveValue, Value>, SlotInstance<Value>>() {

                @Override
                public SlotInstance<Value> closed(ClosedSlotInstance<Either<ReactiveValue, Value>> closedSlotInstance) {
                    return new ClosedSlotInstance<Value>(ReactiveValue.currentValue(trace, closedSlotInstance.value));
                }

                @Override
                public SlotInstance<Value> open(OpenSlotInstance<Either<ReactiveValue, Value>> openSlotInstance) {
                    return new OpenSlotInstance<Value>(openSlotInstance.name, openSlotInstance.slotObjectRef, openSlotInstance.body,
                        openSlotInstance.closure.map(e -> ReactiveValue.currentValue(trace, e)));
                }

            })));
    }

}
