package banjo.eval.signal;

import banjo.eval.expr.ClosedSlotInstance;
import banjo.eval.expr.ObjectLiteralInstance;
import banjo.eval.expr.OpenSlotInstance;
import banjo.eval.expr.SlotInstance;
import banjo.eval.expr.SlotInstanceVisitor;
import banjo.eval.resolver.NameRef;
import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.TreeMap;

public class ObjectLiteralSignal implements Signal {

    protected static final Ord<ObjectLiteralSignal> ORD = Ord
            .streamOrd(Ord.p2Ord(Ord.stringOrd, SlotInstance.ord(SignalOrd.ORD)))
            .contramap((x) -> x.slots.toStream());

    public final Set<SourceFileRange> ranges;
    public final TreeMap<String, SlotInstance<Signal>> slots;

    public ObjectLiteralSignal(Set<SourceFileRange> ranges, TreeMap<String, SlotInstance<Signal>> slots) {
        this.ranges = ranges;
        this.slots = slots;
    }

    @Override
    public <T> T acceptVisitor(SignalVisitor<T> visitor) {
        return visitor.objectLiteral(this);
    }

    public SignalNetwork slotNetwork(SlotInstance<Signal> slot) {
        return slot.acceptVisitor(new SlotInstanceVisitor<Signal, SignalNetwork>() {
            @Override
            public SignalNetwork closed(ClosedSlotInstance<Signal> closedSlotInstance) {
                return closedSlotInstance.value.connect(ObjectLiteralSignal.this);
            }

            @Override
            public SignalNetwork open(OpenSlotInstance<Signal> openSlotInstance) {
                return openSlotInstance.closure.values()
                        .foldLeft((a, b) -> a.merge(b.connect(ObjectLiteralSignal.this)), SignalNetwork.EMPTY);
            }
        });
    }

    @Override
    public SignalNetwork network() {
        return slots.values().foldLeft((a, b) -> a.merge(slotNetwork(b)), SignalNetwork.EMPTY);
    }

    public SlotInstance<Value> valueSlotInstance(SlotInstance<SignalUpdate> sisu, long t) {
        return sisu.acceptVisitor(new SlotInstanceVisitor<SignalUpdate, SlotInstance<Value>>() {

            @Override
            public SlotInstance<Value> closed(ClosedSlotInstance<SignalUpdate> si) {
                return SlotInstance.closed(si.value.getValue(t));
            }

            @Override
            public SlotInstance<Value> open(OpenSlotInstance<SignalUpdate> openSlotInstance) {
                TreeMap<NameRef, Value> valueClosure = openSlotInstance.closure.map((su) -> su.getValue(t));
                return new OpenSlotInstance<Value>(openSlotInstance.name, openSlotInstance.slotObjectRef,
                        openSlotInstance.body, valueClosure);
            }
        });
    }

    public SignalUpdate signalUpdate(List<P2<String, SlotInstance<SignalUpdate>>> slotUpdates) {
        Iterable<SignalUpdate> sourceUpdates = List.join(slotUpdates.map(P2.__2()).map((si) -> si.acceptVisitor(new SlotInstanceVisitor<SignalUpdate, List<SignalUpdate>>() {
                    @Override
                    public List<SignalUpdate> closed(ClosedSlotInstance<SignalUpdate> closedSlotInstance) {
                        return List.single(closedSlotInstance.value);
                    }

                    @Override
                    public List<SignalUpdate> open(OpenSlotInstance<SignalUpdate> openSlotInstance) {
                        return openSlotInstance.closure.values();
                    }
            
                })));

        return new SignalUpdate() {

            @Override
            public Value getValue(long t) {
                TreeMap<String, SlotInstance<Value>> slotValues = TreeMap.iterableTreeMap(Ord.stringOrd,
                        slotUpdates.map((P2<String, SlotInstance<SignalUpdate>> p) -> p
                                .map2((SlotInstance<SignalUpdate> siu) -> valueSlotInstance(siu, t))));
                return new ObjectLiteralInstance(ranges, slotValues);
            }

            @Override
            public java.lang.Iterable<SignalUpdate> getSourceUpdates() {
                return sourceUpdates;
            };
        };
    }

    public Option<SlotInstance<SignalUpdate>> slotInstanceUpdate(SlotInstance<Signal> si, SignalNetwork network) {
        return si.acceptVisitor(new SlotInstanceVisitor<Signal, Option<SlotInstance<SignalUpdate>>>() {
            @Override
            public Option<SlotInstance<SignalUpdate>> closed(ClosedSlotInstance<Signal> si) {
                return si.value.update(network).map(SlotInstance::closed);
            }

            @Override
            public Option<SlotInstance<SignalUpdate>> open(OpenSlotInstance<Signal> openSlotInstance) {
                Option<List<P2<NameRef, SignalUpdate>>> closure1 = openSlotInstance.closure.toList()
                        .foldRight(
                                (P2<NameRef, Signal> p, Option<List<P2<NameRef, SignalUpdate>>> olsu) -> olsu
                                        .bind((List<P2<NameRef, SignalUpdate>> lsu) -> network.values.get(p._2())
                                                .map((SignalUpdate su) -> lsu.cons(P.p(p._1(), su)))),
                                Option.some(List.nil()));
                return closure1.map((closureUpdates) -> new OpenSlotInstance<SignalUpdate>(openSlotInstance.name,
                        openSlotInstance.slotObjectRef, openSlotInstance.body,
                        TreeMap.iterableTreeMap(NameRef.ORD, closureUpdates)));
            }
        });
    }
    @Override
    public Option<SignalUpdate> update(SignalNetwork network) {
        Option<List<P2<String, SlotInstance<SignalUpdate>>>> slotUpdates = slots.toList()
                .foldRight(
                        (P2<String, SlotInstance<Signal>> p,
                                Option<List<P2<String, SlotInstance<SignalUpdate>>>> olsu) -> olsu.bind(
                                        (List<P2<String, SlotInstance<SignalUpdate>>> lsu) -> slotInstanceUpdate(p._2(),
                                                network)
                        .map((siu) -> lsu.cons(P.p(p._1(), siu)))),
                Option.some(List.nil()));
        return slotUpdates.map(this::signalUpdate);
    }
}
