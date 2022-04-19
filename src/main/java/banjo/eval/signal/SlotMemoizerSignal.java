package banjo.eval.signal;

import banjo.value.Value;
import banjo.value.special.SlotMemoizer;
import fj.Ord;
import fj.data.List;
import fj.data.Option;

public class SlotMemoizerSignal implements Signal {
    public static class SlotMemoizerSignalUpdate implements SignalUpdate {
        public final SignalUpdate targetUpdate;

        public SlotMemoizerSignalUpdate(SignalUpdate targetUpdate) {
            super();
            this.targetUpdate = targetUpdate;
        }

        @Override
        public Value getValue(long t) {
            return new SlotMemoizer(targetUpdate.getValue(t));
        }

        @Override
        public long getTimestamp() {
            return targetUpdate.getTimestamp();
        }

        @Override
        public boolean isTimeDependent() {
            return targetUpdate.isTimeDependent();
        }

        @Override
        public Iterable<SignalUpdate> getSourceUpdates() {
            return List.single(targetUpdate);
        }
    }

    protected static final Ord<SlotMemoizerSignal> ORD = SignalOrd.ORD.contramap((s) -> s.target);
    public final Signal target;

    public SlotMemoizerSignal(Signal target) {
        super();
        this.target = target;
    }

    @Override
    public <T> T acceptVisitor(SignalVisitor<T> visitor) {
        return visitor.slotMemoizer(this);
    }

    @Override
    public SignalNetwork network() {
        return target.connect(this);
    }

    @Override
    public Option<SignalUpdate> update(SignalNetwork network) {
        return target.update(network).map(SlotMemoizerSignalUpdate::new);
    }

}
