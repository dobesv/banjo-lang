package banjo.eval;

import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import fj.data.List;
import fj.data.Set;

public class FailWithSourceFileRangesAndMessage extends FailWithMessage {
    private final Set<SourceFileRange> ranges;

    public FailWithSourceFileRangesAndMessage(List<Value> trace, String message, Set<SourceFileRange> ranges) {
        super(trace, message);
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
