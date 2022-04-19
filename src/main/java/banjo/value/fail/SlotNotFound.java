package banjo.value.fail;

import banjo.eval.EvalContext;
import banjo.expr.util.SourceFileRange;
import fj.data.Set;


public class SlotNotFound<T> extends Fail {
	public final String id;
    private final Set<SourceFileRange> ranges;
    public final T object;
	public final Throwable cause;

    public SlotNotFound(EvalContext<T> ctx, String id, Set<SourceFileRange> ranges, T object) {
        this(ctx, id, ranges, object, null);
    }

    public SlotNotFound(EvalContext<T> ctx, String id, Set<SourceFileRange> ranges, T object, Throwable cause) {
        super(ctx);
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