package banjo.eval.signal;

import java.util.Arrays;

import banjo.expr.util.OrdUtil;
import banjo.value.Value;
import fj.Ord;
import fj.data.Option;

public class ExtendedObjectSignal implements Signal {
    public static final Ord<ExtendedObjectSignal> ORD = OrdUtil.chain(SignalOrd.ORD.contramap((eos) -> eos.base),
            SignalOrd.ORD.contramap((eos) -> eos.extension));

    Signal base;
    Signal extension;

    public ExtendedObjectSignal(Signal base, Signal extension) {
        super();
        this.base = base;
        this.extension = extension;
    }

    @Override
    public <T> T acceptVisitor(SignalVisitor<T> visitor) {
        return visitor.extendedObject(this);
    }

    @Override
    public SignalNetwork network() {
        return base.network().merge(extension.network());
    }

    public SignalUpdate signalUpdate(SignalUpdate baseUpdate, SignalUpdate extensionUpdate) {
        return new SignalUpdate() {

            @Override
            public Value getValue(long t) {
                Value baseValue = baseUpdate.getValue(t);
                Value extensionValue = extensionUpdate.getValue(t);
                return baseValue.extendedWith(extensionValue);
            }

            @Override
            public long getTimestamp() {
                return Math.max(baseUpdate.getTimestamp(), extensionUpdate.getTimestamp());
            }

            @Override
            public boolean isTimeDependent() {
                return baseUpdate.isTimeDependent() || extensionUpdate.isTimeDependent();
            }

            @Override
            public Iterable<SignalUpdate> getSourceUpdates() {
                return Arrays.asList(baseUpdate, extensionUpdate);
            }
        };
    }

    @Override
    public Option<SignalUpdate> update(SignalNetwork network) {
        return base.update(network).bind((baseUpdate) -> extension.update(network)
                .map((extensionUpdate) -> signalUpdate(baseUpdate, extensionUpdate)));
    }
}
