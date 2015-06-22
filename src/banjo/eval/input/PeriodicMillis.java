package banjo.eval.input;

public class PeriodicMillis implements InputValue {
	public final long period;
	long lastPollTime = System.currentTimeMillis();

	public PeriodicMillis(long period) {
	    super();
	    this.period = period;
    }

	@Override
	public int compareTo(InputValue o) {
		if(o == this) return 0;
		int cmp = o.getClass().getName().compareTo(getClass().getName());
		if(cmp != 0) return cmp;
		return Long.compare(period, ((PeriodicMillis)o).period);
	}

	@Override
	public long getNextPollTime() {
		return lastPollTime + period;
	}

	public long currentFrameTime() {
	    return (System.currentTimeMillis() / period) * period;
    }

	@Override
	public Object currentValue() {
	    return Long.valueOf(lastPollTime = currentFrameTime());
	}
}
