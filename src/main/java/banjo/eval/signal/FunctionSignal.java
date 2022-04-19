package banjo.eval.signal;

import banjo.eval.expr.FunctionInstance;
import banjo.eval.resolver.NameRef;
import banjo.expr.free.FreeExpression;
import banjo.expr.util.OrdUtil;
import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.TreeMap;

public class FunctionSignal implements Signal {
    public static final Ord<FunctionSignal> ORD = OrdUtil.chain(
            Ord.listOrd(Ord.stringOrd).contramap(FunctionSignal::getArgs),
            FreeExpression.ORD.contramap(FunctionSignal::getBody),
            Ord.optionOrd(Ord.stringOrd).contramap(FunctionSignal::getCalleeBinding),
            SignalOrd.ORD.contramap(FunctionSignal::getTrait), Ord.streamOrd(Ord.p2Ord(NameRef.ORD, SignalOrd.ORD))
                    .contramap((fs) -> fs.closure.toStream()));
    public final Set<SourceFileRange> ranges;
    public final List<String> args;
    public final FreeExpression body;
    public final Option<String> calleeBinding;
    public final Signal trait;
    public final TreeMap<NameRef, Signal> closure;

    public FunctionSignal(Set<SourceFileRange> ranges, List<String> args, FreeExpression body,
            Option<String> calleeBinding, Signal trait, TreeMap<NameRef, Signal> closure) {
        this.ranges = ranges;
        this.args = args;
        this.body = body;
        this.calleeBinding = calleeBinding;
        this.trait = trait;
        this.closure = closure;
    }

    @Override
    public <T> T acceptVisitor(SignalVisitor<T> visitor) {
        return visitor.function(this);
    }

    @Override
    public SignalNetwork network() {
        return closure.values().foldLeft((network, s) -> network.merge(s.connect(this)), SignalNetwork.EMPTY);
    }

    public SignalUpdate signalUpdate(List<P2<NameRef, SignalUpdate>> closureUpdates, SignalUpdate traitUpdate) {
        Iterable<SignalUpdate> sourceUpdates = closureUpdates.map(P2.__2()).cons(traitUpdate);
        return new SignalUpdate() {
            
            @Override
            public Value getValue(long t) {
                TreeMap<NameRef, Value> valueClosure = TreeMap.iterableTreeMap(NameRef.ORD, closureUpdates.map((p) -> p.map2((su) -> su.getValue(t))));
                return new FunctionInstance(ranges, args, body, calleeBinding, traitUpdate.getValue(t), valueClosure);
            }

            @Override
            public Iterable<SignalUpdate> getSourceUpdates() {
                return sourceUpdates;
            }
        };
    }

    @Override
    public Option<SignalUpdate> update(SignalNetwork network) {
        Option<List<P2<NameRef, SignalUpdate>>> closure1 = closure.toList()
                .foldRight(
                        (P2<NameRef, Signal> p, Option<List<P2<NameRef, SignalUpdate>>> olsu) -> olsu
                        .bind((List<P2<NameRef, SignalUpdate>> lsu) -> network.values.get(p._2())
                                        .map((SignalUpdate su) -> lsu.cons(P.p(p._1(), su)))),
                Option.some(List.nil()));
        Option<SignalUpdate> trait1 = trait.update(network);
        return trait1.bind(
                (traitUpdate) -> closure1.map((closureUpdates) -> this.signalUpdate(closureUpdates, traitUpdate)));
    }

    public Set<SourceFileRange> getRanges() {
        return ranges;
    }

    public List<String> getArgs() {
        return args;
    }

    public FreeExpression getBody() {
        return body;
    }

    public Option<String> getCalleeBinding() {
        return calleeBinding;
    }

    public Signal getTrait() {
        return trait;
    }

    public TreeMap<NameRef, Signal> getClosure() {
        return closure;
    }

}
