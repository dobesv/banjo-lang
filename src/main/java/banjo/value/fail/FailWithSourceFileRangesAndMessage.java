package banjo.value.fail;

import banjo.eval.EvalContext;
import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public class FailWithSourceFileRangesAndMessage extends FailWithMessage {
    private final Set<SourceFileRange> ranges;

    public FailWithSourceFileRangesAndMessage(EvalContext<?> ctx, Set<SourceFileRange> ranges, String message) {
        super(ctx, message);
        this.ranges = ranges;
    }

    @Override
    public Set<SourceFileRange> getRanges() {
        return ranges;
    }

    @Override
    public String getMessage() {
        if(!ranges.isEmpty())
            return ranges.iterator().next() + ": " + message;
        return message;
    }
}
