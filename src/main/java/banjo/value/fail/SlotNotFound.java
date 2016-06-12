package banjo.value.fail;

import banjo.expr.util.SourceFileRange;
import fj.data.List;
import fj.data.Set;


public class SlotNotFound<T> extends Fail {
	public final String id;
    private final Set<SourceFileRange> ranges;
    public final T object;
	public final Throwable cause;

    public SlotNotFound(List<T> trace, String id, Set<SourceFileRange> ranges, T object) {
        this(trace, id, ranges, object, null);
    }

    public SlotNotFound(List<T> trace, String id, Set<SourceFileRange> ranges, T object, Throwable cause) {
        super(trace);
		this.id = id;
        this.ranges = ranges;
		this.object = object;
		this.cause = cause;
    }

	@Override
	public String getMessage() {
        String msg = "No slot named '" + id + "' in " + object;
        if(!getRanges().isEmpty()) {
            msg = getRanges().iterator().next() + ": " + msg;
        }
        return msg;
	}

	@Override
	public Throwable getCause() {
		return cause;
	}

    @Override
    public Set<SourceFileRange> getRanges() {
        return ranges;
    }
}