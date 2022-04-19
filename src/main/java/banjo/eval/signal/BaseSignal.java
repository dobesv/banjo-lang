package banjo.eval.signal;

import banjo.eval.EvalContext;
import banjo.expr.token.Identifier;
import banjo.value.Value;
import fj.data.TreeMap;

public abstract class BaseSignal implements Signal {

    private final Identifier id;
    public TreeMap<Identifier, Signal> sinks = TreeMap.empty(Identifier.ORD);

    public long cachedValueAsOf;
    public Value cachedValueProjectRoot;
    public Value cachedValue;

    public BaseSignal(Identifier id) {
        super();
        this.id = id;
    }

    public abstract Iterable<Signal> getSources();

    @Override
    public int getRank() {
        int height = 0;
        for (Signal s : getSources()) {
            height = Math.max(height, s.getRank() + 1);
        }
        return height;
    }

    @Override
    public void connect(Signal sink) {
        if (sinks.isEmpty()) {
            getSources().forEach((s) -> s.connect(this));
        }
        sinks = sinks.set(sink.getId(), sink);
    }

    @Override
    public void disconnect(Signal sink) {
        sinks = sinks.delete(sink.getId());
        if (sinks.isEmpty()) {
            getSources().forEach((s) -> s.disconnect(this));
        }
    }
    @Override
    public Identifier getId() {
        return id;
    }

    @Override
    public Value getValue(EvalContext<Value> ctx, long asOf) {
        if (asOf != cachedValueAsOf || cachedValueProjectRoot != ctx.projectRoot) {
            cachedValueAsOf = asOf;
            cachedValueProjectRoot = ctx.projectRoot;
            cachedValue = this.calculateValue(ctx, asOf);
        }
        return cachedValue;
    }

    protected abstract Value calculateValue(EvalContext<Value> ctx, long asOf);
}
