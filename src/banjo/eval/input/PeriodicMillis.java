package banjo.eval.input;

public class PeriodicMillis implements InputValue {
	public final long period;
	long frameStart = System.currentTimeMillis();

	public PeriodicMillis(long startTime, long period) {
	    super();
	    this.frameStart = startTime;
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
	public long getNextPollTime(long currentTimeMillis) {
		return Math.max(frameStart + period, currentTimeMillis);
	}

	@Override
	public Object currentValue() {
	    return Long.valueOf(frameStart);
	}

	@Override
	public void poll(long currentTimeMillis) {
		frameStart = getNextPollTime(currentTimeMillis);
	}
}
