package banjo.eval.signal;

import java.util.function.Consumer;

import banjo.eval.EvalContext;
import banjo.expr.token.Identifier;
import banjo.value.Value;
import fj.data.Set;

public interface Signal {

    Set<Signal> EMPTY_SET = Set.empty(SignalOrd.ORD);

    /**
     * Because signals are mutable entities we need an identifier for them.
     */
    public Identifier getId();

    public <T> T acceptVisitor(SignalVisitor<T> visitor);

    /**
     * The ultimate purpose of the signal is to let us listen for changes to the
     * signal state, which gives us a function we can use to retrieve the value
     * of the signal at a given time.
     * 
     * @param listener
     */
    public default void listen(Consumer<SignalUpdate> listener) {
        // TODO
    }

    /**
     * When a signal listens to another signal we construct an inverse
     * dependency graph / signal network.
     */
    public void connect(Signal sink);

    /**
     * Remove a sink
     */
    public void disconnect(Signal sink);

    /**
     * Return the maximum depth of the tree of signals this signal depends on.
     * This is used to order signal updates such that all signals this one
     * depends on are updated before it is.
     */
    public int getRank();

    /**
     * Request the value of the signal. Note that the `asOf` parameter should
     * always represent the current moment - it is only provided so that all
     * time-based calculations use the same clock value. Passing future or past
     * values at best will extrapolate or interpolate according to the current
     * state of the signals.
     * 
     * @param asOf
     *            Time to use for time-based interpolation / extrapolation
     * @return Signal value
     */
    public Value getValue(EvalContext<Value> ctx, long asOf);
}
