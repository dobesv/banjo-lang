package banjo.eval.signal;

import banjo.value.Value;
import fj.data.List;

public interface SignalUpdate {
    /**
     * Get the time of the update. The time unit here is determined by the clock
     * being used, it could be milliseconds or it could be the number of frames.
     * 
     * The default implementation returns the maximum timestamp of the signal
     * updates this update depends on.
     */
    public default long getTimestamp() {
        long ts = 0;
        for(SignalUpdate sourceUpdate : this.getSourceUpdates()) {
            ts = Math.max(ts, sourceUpdate.getTimestamp());
        }
        return ts;
    }

    /**
     * Return true if the value returned by getValue() actually depends on the
     * time parameter given to it, or just on the underlying signal states.
     */
    public default boolean isTimeDependent() {
        boolean isTimeDependent = false;
        for (SignalUpdate sourceUpdate : this.getSourceUpdates()) {
            isTimeDependent = isTimeDependent || sourceUpdate.isTimeDependent();
        }
        return isTimeDependent;
    }

    /**
     * True if this signal is not expected to update again and the signal be
     * pruned from the network after delivering this update.
     */
    public default boolean isFinal() {
        boolean isFinal = true;
        for (SignalUpdate sourceUpdate : this.getSourceUpdates()) {
            isFinal = isFinal && sourceUpdate.isFinal();
        }
        return isFinal;
    }

    /**
     * Get a list of signal updates from the signals the underlying signal
     * depends on. Used to share the implemtnation of default methods above.
     */
    public Iterable<SignalUpdate> getSourceUpdates();

    /**
     * Get the value of the sign as of the given time. If the signal supports
     * interpolation / extrapolation, its value may vary over time between
     * updates.
     * 
     * Note that this only considers the current state, so requesting a time
     * prior to this update's timestamp, or after the next update's timestamp
     * won't account for the past / future state of the signal.
     */
    public Value getValue(long t);

    /**
     * Return a signal update that has a non-time-dependent value.
     * 
     * @param t
     *            Timestamp of the update
     * @param v
     *            Value to return from getValue()
     * @return A SignalUpdate value
     */
    public static SignalUpdate value(long t, Value v) {
        return new SignalUpdate() {

            @Override
            public Value getValue(long t) {
                return v;
            }

            @Override
            public long getTimestamp() {
                return t;
            }

            @Override
            public boolean isTimeDependent() {
                return false;
            }
            
            @Override
            public Iterable<SignalUpdate> getSourceUpdates() {
                return List.nil();
            }
        };
    }
}