package banjo.eval;

import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import fj.data.List;
import fj.data.Set;

public class UnboundSlotSelfName extends FailWithSourceFileRangesAndMessage {

    public UnboundSlotSelfName(List<Value> trace, String message, Set<SourceFileRange> ranges) {
        super(trace, message, ranges);
    }

}